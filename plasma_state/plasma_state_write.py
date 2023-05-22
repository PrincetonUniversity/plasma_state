#!/usr/bin/env python3

#--------------------------------------------------------------------------
# plasma_state_write
# write the generated code pieces based on the plasma state specification
#
# two externally accessed procedures:
#   plasma_state_proc_header
#     -- processors header text; inserts this and generates header for
#        all output files
#
#   plasma_state_write
#     -- finish writing the output files
#
#--------------------------------------------------------------------------
#  shared variables from plasma_state_global...
#  error handling routines from plasma_state_sub also used...
#--------------------------------------------------------------------------

import os

#--------------------------------------------------------------------------
# global items...

from plasma_state_global import *

#--------------------------------------------------------------------------
# utility routines and error handling...

from plasma_state_sub import *

#--------------------------------------------------------------------------
# process header data
#
#  see corresponding section of plasma_state_state_spec.dat file

def plasma_state_proc_header(vid,text,section):

    global version_id

    version_id = vid

    # generate HEADERS

    print("...write output file headers...")

    # this means: declarations for each generated fortran source file
    # portions of this that are in common are passed as the array "text":
    #   "use" statements followed by IMPLICIT NONE.

    #-------------------------------
    # plasma_state_constants_mod

    f_const.write("""
MODULE plasma_state_constants_mod

  ! plasma_state header:

""")
    for line in text:
        f_const.write("  " + line + "\n")

    f_const.write("""

  PRIVATE

  !--------------------------------------------
  !
  !  PLASMA STATE MODULE -- CONSTANTS (PARAMETER statements)
  !
  !--------------------------------------------

""")

    f_const.write('  CHARACTER*32, PARAMETER, PUBLIC :: ps_version_id = "'
                  + vid + '"\n\n')

    #-------------------------------
    # plasma_state_util_mod

    f_util.write("""
MODULE plasma_state_util_mod

  ! plasma_state header:

""")
    for line in text:
        f_util.write("  " + line + "\n")

    f_util.write("""

  SAVE
  PRIVATE

  !--------------------------------------------
  !
  !  PLASMA STATE MODULE -- UTILITY VARIABLES
  !    debug switch; counter for each defined component.
  !
  !--------------------------------------------
  !  The only SAVEd global variables are:
  !     ps_debug
  !
  !  All the rest are PARAMETERs.
  !--------------------------------------------

  INTEGER, PUBLIC :: ps_debug = 0  ! set =1 for summary debugging;
  ! set ps_debug = 2 for detailed debugging of plasma_state_store operations.

  INTEGER, PARAMETER, PUBLIC :: ps_ignore = 0
  INTEGER, PARAMETER, PUBLIC :: ps_warn = 1
  INTEGER, PARAMETER, PUBLIC :: ps_error = 2

  PUBLIC :: ps_diff
  PUBLIC :: ps_diff_c0,ps_diff_c1
  PUBLIC :: ps_diff_i0,ps_diff_i1,ps_diff_i2,ps_diff_i3
  PUBLIC :: ps_diff_r0,ps_diff_r1,ps_diff_r2

  INTERFACE ps_diff
     MODULE PROCEDURE ps_diff_c0,ps_diff_c1
     MODULE PROCEDURE ps_diff_i0,ps_diff_i1,ps_diff_i2,ps_diff_i3
     MODULE PROCEDURE ps_diff_r0,ps_diff_r1,ps_diff_r2
  END INTERFACE

  PUBLIC :: ps_conformal
  PUBLIC :: ps_conformal_c1
  PUBLIC :: ps_conformal_i1,ps_conformal_i2,ps_conformal_i3
  PUBLIC :: ps_conformal_r1,ps_conformal_r2,ps_conformal_r3,ps_conformal_r4

  INTERFACE ps_conformal
     MODULE PROCEDURE ps_conformal_c1
     MODULE PROCEDURE ps_conformal_i1,ps_conformal_i2,ps_conformal_i3
     MODULE PROCEDURE ps_conformal_r1,ps_conformal_r2,ps_conformal_r3,ps_conformal_r4
  END INTERFACE

""")

    #-------------------------------
    # plasma_state_definition_mod

    f_sdef.write("""
MODULE plasma_state_definition_mod

#include "ps_container_size.h"

  USE plasma_state_constants_mod
  USE plasma_state_util_mod

  ! plasma_state header:

""")
    for line in text:
        f_sdef.write("  " + line + "\n")

    f_sdef.write("""

  !--------------------------------------------
  !
  !  PLASMA STATE DATA TYPE
  !
  !--------------------------------------------

  !  assumed size (#integer words) of a plasma_state_container instance...
  !  Note: use MACRO defined in #include file!

  INTEGER, PARAMETER :: ps_container_size = __PS_CONTAINER_SIZE

  TYPE :: plasma_state_container

     TYPE (plasma_state), pointer :: ps_ptr => NULL()

  END TYPE plasma_state_container

  TYPE :: plasma_state

     ! version of the software (i.e. that wrote state data to a file)

     CHARACTER*32 :: version_ID

     ! ---------------------------------------
     ! internal tags for use of software that manages collections of
     ! plasma states.  These quantities are not read from or written to
     ! disk files not copied between state objects!  these must be
     ! maintained by the module or software which instantiates state
     ! objects.

     INTEGER :: itag = ps_unInit
     CHARACTER*32 :: chtag = ' '

     ! this is an opaque container for an interpolation object-- a
     ! separate such object to be associated with each state instance.
     !  Note: use MACRO defined in #include file!

     INTEGER :: iobj(ps_container_size) = __PS_CONTAINER_INIT

     ! as of Jan. 2008 "xplasma" provides the interpolation capability
     ! but in principle this could be replaced.  This object will imbed
     ! a pointer, and it cannot be safely copied, nor read from nor written
     ! to disk files!  See "plasma_state_mod.f90" for usage.

     ! ---------------------------------------
     ! zone weight mapping index
     ! if ID_ZONWT(id) is ZERO, the weighting is just volume or area
     ! depending on the profile type; if it is non-zero, it is an ID
     ! of a profile over the same grid that is to be used for weighting
     ! (for example, when Temperatures are rezoned a density weighting
     ! would be used; ID_ZONWT(id) will point to the correct density profile).
     ! This is updated when the state is updated or read from file.

     INTEGER, dimension(:), allocatable :: ID_ZONWT

     ! ---------------------------
     ! hash codes
     ! These are used to detect that a profile, grid, or set of variables
     ! contains changes (e.g. when just read in from a file).

     INTEGER, dimension(ps_hash_total) :: hash_codes      ! file copy
     INTEGER, dimension(ps_hash_total) :: new_hash_codes  ! updated..
     INTEGER, dimension(ps_hash_total) :: saved_hash_codes  ! saved..

     ! ---------------------------
     ! count of active dimensions in each component

     INTEGER, dimension(ps_ccount) :: active_dims

     ! --------------------------------------------------------------------

""")

    #-------------------------------
    # plasma_state_store

    f_xput.write("""
SUBROUTINE plasma_state_store(filename,ss,icheck,ierr)

  ! store (i.e. write to file) plasma state

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! state file
  TYPE (plasma_state) :: ss     ! state data to write
  INTEGER, INTENT(in) :: icheck ! =0 to skip equilibrium check
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  ! if filename is blank, plasma state is loaded into xplasma
  ! and hash codes are updated, but, no NetCDF file is written.
  !-----------------------

  INTEGER :: icdf,iout,varid,dimid,status,iertot,ival,ii,isize
  INTEGER :: idims(20),indim
  CHARACTER*80 :: nc_errmsg
  CHARACTER*32 :: dimname

  !-----------------------
  ! Initialization...

  iout = SWIM_err_out_file

  ss%version_id = ps_version_id

  !------------------------------------
  ! reload equilibrium and other data in xplasma, with checks, as needed...
  ! ...hand maintained code orchestrates:
  !     0. plasma_state_empty_alloc; update ps_active_dims(...)
  !     1. compute new_hash_codes
  !     2. xplasma update of equilibrium as needed based on hash codes
  !     3. xplasma update of data and profiles as needed
  !     4. "zonwt" update
  !     5. update of derived quantities in xplasma.

  CALL ps_xload_check(ss,iout,icheck,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error in ps_xload_check."
     ierr = 9999
     RETURN
  ENDIF

  CALL plasma_state_xload(ss,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?plasma_state_store: error in plasma_state_xload."
     ierr = 9999
     RETURN
  ENDIF

  !------------------------------------
  ! in this routine, everything is written out, so no need to keep
  ! old hash codes.

  ss%hash_codes = ss%new_hash_codes

  !------------------------------------
  ! if no filename is provided, we are done!

  IF(filename.eq.' ') RETURN
  !------------------------------------
  ! if a filename is provided, create (clobber!) a new NetCDF file

  status = nf90_create(trim(filename),NF90_CLOBBER,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_store: file create failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  !------------------------------------
""")

    #-------------------------------
    # ps_update_report_exec

    f_uprp.write("""
SUBROUTINE ps_update_report_exec(ss,cclist,isect,ireverse,iout,ierr)

  ! generate report on Plasma State quantities that have been modified,
  ! based on hash code analysis.

  ! report for elements in state (ss)

  ! covering sections per (isect) and components per (cclist)

  ! if ireverse.eq.1 write list of items NOT updated;
  !    otherwise write list of updated items.

  ! write ascii report to fortran logical unit (iout)

  ! return status code (ierr)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(IN) :: cclist(ps_ccount) ! component-by-component control

  INTEGER, INTENT(IN) :: isect  ! section code
    ! =1: State_Profiles section ONLY
    ! =2: State_Profiles & State_Data sections
    ! =3: All sections

  INTEGER, INTENT(IN) :: ireverse  ! =1 to list NOT-updated quantities
  ! otherwise, list updated quantities.

  INTEGER, INTENT(IN) :: iout   ! I/O unit for output
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  LOGICAL :: plasma_state_hash_match,ichange

  INTEGER :: jout,jsect,iaddr,iertmp
  CHARACTER*20 :: hdr

  LOGICAL :: iwrite

  !-----------------------

  jout = SWIM_err_out_file
  jsect = max(3,min(1,isect))

  call plasma_state_hash_code(ss,cclist,ierr)
  if(ierr.NE.0) then
     write(iout,*) ' ?ps_update_report_exec: unexpected hash code error! '
     write(jout,*) ' ?ps_update_report_exec: unexpected hash code error! '
     return
  endif

  if(ireverse.EQ.1) then
     hdr = "NOT updated:"
  else
     hdr = "updated:"
  endif

""")

    #-------------------------------
    # ps_update_file_write

    f_upwr.write("""
SUBROUTINE ps_update_file_write(filename,ss,cclist,iwarn,ierr)

  ! store (i.e. write to file) plasma state updates
  ! determine what to write as follows:
  !   (a) scalar data:  each item with a change in value is written; if any
  !       element of an array of scalars has changes the entire array is
  !       written.
  !   (b) profile data:  each array that contains ANY changes is written;
  !       Note the entire array is written if there are changes to any parts.
  !   (c) array dimensions needed to define arrays to be written... are also
  !       written.
  !
  ! dimensioning information necessary to support this data is also written.
  ! attributes are NOT written-- these are in the full state files only.

  USE plasma_state_definition_mod
  USE netcdf
  USE ps_nidlist_defn
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! update file
  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(IN) :: cclist(ps_ccount) ! component-by-component control

  INTEGER, INTENT(OUT) :: iwarn ! number of modified items not written
  !  this would be due to a modification in an item belonging to component XX
  !  for which cclist(ps_cnum_XX).ne.1; warning messages are printed.

  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  LOGICAL :: plasma_state_hash_match,ichange

  TYPE (nidlist) :: nids

  INTEGER :: icdf,iout,varid,pvarid,vvarid,dimid,status,iertot,iertmp,iaddr
  INTEGER :: idims(20),indim,ival,iucount,ii
  CHARACTER*80 :: nc_errmsg

""")
    ff = open("ps_nidlist_defn.f90","w")
    ff.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

MODULE ps_nidlist_defn

  TYPE :: nidlist

"""%version_id)

    for dim in all_dims:
        ddescr = all_items[dim]
        name = ddescr['name']
        ilist = ddescr['ilist']
        for incr in ilist:
            ncdname = gen_dname(incr,name)
            ff.write('     INTEGER :: nid_' + ncdname + '\n')

    for iclen in char_lens:
        idname = 'nid_clen_' + repr(iclen)
        ff.write('     INTEGER :: ' + idname + '\n')

    ff.write("""

  END TYPE nidlist

END MODULE ps_nidlist_defn
""")
    ff.close()

    f_upwr.write('\n')
    f_upwr.write('  !-----------------------\n')
    f_upwr.write('  ! Initialization...\n')
    f_upwr.write('\n')

    for dim in all_dims:
        ddescr = all_items[dim]
        name = ddescr['name']
        ilist = ddescr['ilist']
        for incr in ilist:
            ncdname = gen_dname(incr,name)
            f_upwr.write('  nids%nid_' + ncdname + ' = 0\n')

    for iclen in char_lens:
        idname = 'nid_clen_' + repr(iclen)
        f_upwr.write('  nids%' + idname + ' = 0\n')

    f_upwr.write("""
  iout = SWIM_err_out_file

  iucount = 0

  iwarn = 0

  !------------------------------------
  ! reload equilibrium and other data in xplasma, with checks, as needed...
  ! ...hand maintained code orchestrates:
  !     0. plasma_state_empty_alloc; update ps_active_dims(...)
  !     1. compute new_hash_codes
  !     2. xplasma update of equilibrium as needed based on hash codes
  !     3. xplasma update of data and profiles as needed
  !     4. "zonwt" update
  !     5. update of derived quantities in xplasma.

  CALL ps_xload_check(ss,iout,1,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) " ?ps_update_file_write: error in ps_xload_check."
     ierr = 9999
     RETURN
  ENDIF

  !------------------------------------
  ! open the updates file...

  status = nf90_create(trim(filename),NF90_CLOBBER,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_write: file create failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  status = nf90_def_var(icdf,"ps_partial_update",NF90_INT, varid=pvarid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_write: "ps_partial_update" variable definition error."'
     ierr=9999
     RETURN
  ENDIF

  !------------------------------------
""")

    #-------------------------------
    # plasma_state_xload

    f_xload.write("""
SUBROUTINE plasma_state_xload(ss,ierr)

  ! store plasma state data in xplasma
  ! enable interpolation services

  ! scalar data is written if section hashes indicate change, or if tests
  ! indicate t

  ! grid data items are written if hashes indicate change, or if grid IDs
  ! cannot be found in xplasma

  ! profile data is written if hashes indicate change of id ss%id_<profile>
  ! variables are zero.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  CHARACTER*32 :: listname,gridname,profname,enum_name,units,list_units
  CHARACTER*80 :: label,plabel,list_label
  INTEGER :: icomponent

  INTEGER :: nitems
  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  INTEGER :: i1,i2,i3,i4,indx,itype,isize
  CHARACTER*32 :: nam1,nam2,nam3,nam4,iiencod,tmpnam,vid_found
  CHARACTER*80 :: ilab1,ilab2,ilab3,ilab4
  INTEGER :: iout,imod,jmod,iconvert,iertmp,iwarn,idchk

  INTEGER :: xhash(ps_hash_total)
  INTEGER :: ahash(ps_max_static)

  !-----------------------
  ! for enumeration labeling...
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: enum_lbl_1d
""")

    for enum in list(all_enum_dims.keys()):
        ref = all_enum_dims[enum]
        if ref != "(index)":
            rdescr = all_items[ref]
            rdims = rdescr['dlist']
            if len(rdims) == 1:
                f_xload.write('  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: enum_lbl_' + ref + '\n')
            else:
                f_xload.write('  CHARACTER*32, DIMENSION(:,:), ALLOCATABLE :: enum_lbl_' + ref + '\n')

    f_xload.write("""
  !-----------------------
  ! for ps_rezone_rho(...)

  REAL(KIND=rspec), dimension(2) :: x01 = (/ 0.0_rspec, 1.0_rspec /)
  INTEGER :: idvec(1),idwvec(1)
  REAL(KIND=rspec) :: ansvec(1,1),dvvec(1)
  INTEGER :: id_intg

  !-----------------------

  iout = SWIM_err_out_file

  CALL ps_xhashes_get(ss%iobj,xhash,iout,ierr)
  IF(ierr.NE.0) RETURN
""")

    #-------------------------------
    # plasma_state_get

    f_xget.write("""
SUBROUTINE plasma_state_get(filename,ss,ierr)

  ! get (i.e. read from file) plasma state
  ! this overwrites any prior contents that might have been in state "ss".

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! state file
  TYPE (plasma_state) :: ss     ! state data to read
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !--------------

  INTEGER :: icdf,iout,varid,dimid,status,iertot,ilen,jlen,idims(20),indim
  INTEGER :: ii,iflag_alloc2,iflag_loc2,isize
  LOGICAL :: version_change
  CHARACTER*80 :: nc_errmsg
  CHARACTER*32 :: dimname
""")

    psw_hash_lock_save(f_xget,"  ","DECLARE","  ")

    f_xget.write("""
  !--------------
  ! Initialization...

  iout = SWIM_err_out_file
  version_change = .FALSE.

  iertot = 0
  CALL ps_init_xpl(ss%iobj,iout,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?plasma_state_get: "ps_init_xpl" error prior to file read.'
     iertot = iertot + 1
  ENDIF

  CALL plasma_state_erase(ss,ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?plasma_state_get: "plasma_state_erase" error prior to file read.'
     iertot = iertot + 1
  ENDIF

  IF(iertot.GT.0) THEN
     ierr=9999
     RETURN
  ENDIF

  ! ********* OPEN the NetCDF file **********

  status = nf90_open(trim(filename),NF90_NOWRITE,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_get: file open failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  ! ********* READ header information
  ! ...check for version_id mismatch

  status = nf90_inq_varid(icdf,"ps_partial_update",varid)
  if(status.EQ.NF90_NOERR) THEN
     WRITE(iout,*) ' ?plasma_state_get: file contains "ps_partial_update"!'
     WRITE(iout,*) '  This routine can only read full states, not partial states.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_inq_varid(icdf,"version_id",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_get: "version_id" not found.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_get_var(icdf,varid,ss%version_id)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?plasma_state_get: "version_id" read error.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF
  CALL denull(ss%version_id)

  IF(ss%version_id.NE.ps_version_id) THEN
     WRITE(iout,*) ' '
     WRITE(iout,*) ' %plasma_state_get: WARNING, version_id mismatch:'
     WRITE(iout,*) '  Version_ID of current software: "'//trim(ps_version_id)//'".'
     WRITE(iout,*) '  Version_ID found in file:       "'//trim(ss%version_id)//'".'
     WRITE(iout,*) ' '
     WRITE(iout,*) '  ...Hash code checks disabled.'
     WRITE(iout,*) '  ...Error handling modified.'
     WRITE(iout,*) ' '
     version_change = .TRUE.
     ss%hash_codes = 0   ! these cannot be used in case of version_change
  ENDIF

  IF(.not.version_change) THEN

     ! hash codes -- these are used to enforce "locks" on data sections
     ! that are meant to be time invariant after initialization...

     status = nf90_inq_varid(icdf,"hash_codes",varid)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr(status)
        WRITE(iout,*) ' ?plasma_state_get: "hash_codes" not found.'
        ierr=9999
        status = nf90_close(icdf)
        RETURN
     ENDIF

     status = nf90_get_var(icdf,varid,ss%hash_codes)
     IF(status.NE.NF90_NOERR) THEN
        CALL ncerr(status)
        WRITE(iout,*) ' ?plasma_state_get: "hash_codes" read error.'
        ierr=9999
        status = nf90_close(icdf)
        RETURN
     ENDIF
  ENDIF

  !----------------------------------------------------
""")

    #-------------------------------
    # ps_update_file_read

    f_uprd.write("""
SUBROUTINE ps_update_file_read(filename,ss,ierr)

  ! get (i.e. read from file) plasma state updates
  ! apply updates to state (ss)

  USE plasma_state_definition_mod
  USE plasma_hash_code
  USE netcdf
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! file containing updates
  TYPE (plasma_state) :: ss     ! state to be updated
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !--------------

  INTEGER :: icdf,iout,varid,dimid,status,iertot
  INTEGER :: ival,jval,icount,iucount,ii
  INTEGER :: prehash1,prehash2
  INTEGER :: posthash1,posthash2
  CHARACTER*80 :: nc_errmsg
  LOGICAL :: izero

  !--------------
  ! Initialization...

  iout = SWIM_err_out_file

  iertot = 0

  iucount = 0

  ! ********* OPEN the NetCDF file **********

  status = nf90_open(trim(filename),NF90_NOWRITE,icdf)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_read: file open failure: "'//trim(filename)//'".'
     ierr=9999
     RETURN
  ENDIF

  ! ********* READ header information
  ! ...check for version_id mismatch

  status = nf90_inq_varid(icdf,"ps_partial_update",varid)
  if(status.NE.NF90_NOERR) THEN
     WRITE(iout,*) ' ?ps_update_file_read: did not find: "ps_partial_update"!'
     WRITE(iout,*) '  This routine can only read state update files, which'
     WRITE(iout,*) '  must contain this marker variable.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_inq_varid(icdf,"version_id",varid)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_read: "version_id" not found.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF

  status = nf90_get_var(icdf,varid,ss%version_id)
  IF(status.NE.NF90_NOERR) THEN
     CALL ncerr(status)
     WRITE(iout,*) ' ?ps_update_file_read: "version_id" read error.'
     ierr=9999
     status = nf90_close(icdf)
     RETURN
  ENDIF
  CALL denull(ss%version_id)

  IF(ss%version_id.NE.ps_version_id) THEN
     WRITE(iout,*) ' %ps_update_file_read: warning: version_id mismatch:'
     WRITE(iout,*) '  Version_ID of current software: "'//trim(ps_version_id)//'".'
     WRITE(iout,*) '  Version_ID found in file:       "'//trim(ss%version_id)//'".'
     WRITE(iout,*) '  (...recovery attempt in progress...)'
  ENDIF

  !--------------------------------------------------------------
""")

    #-------------------------------
    # plasma_state_copy

    f_xcopy.write("""
SUBROUTINE plasma_state_copy(cclist,ss,ss2,idims_only,idata_only, &
                             icopy_lists, iwant_1deq, &
                             ierr)

  ! Copy plasma state ss -> ss2
  !   this routine ONLY copies the state object in memory
  !   a separate step is needed to deal with backing files & interpolation
  !     data.

  ! cclist(...) control specifies the components for which all grids and
  !   profiles are copied.  If cclist(j)=1 all are copied for component j.
  !   If cclist(j)=0, only scalars, species lists, and item lists are copied;
  !   profiles and grids and grid dimensions for component j are not
  !   copied in this case.

  ! if icopy_lists=0, cclist(j)=0 suppresses copy of item lists for
  ! component j, as well as profiles and grids.  Note that species lists
  ! are shared accross components; copying cannot be suppressed.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to fully copy
                                ! use ps_cc_all to copy all components

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*

  INTEGER, INTENT(IN) :: idims_only  ! =1 to copy dimensions ONLY: no alloc

  INTEGER, INTENT(IN) :: idata_only  ! =1 to copy data ONLY: non-zero dims
                                     ! MUST match
                                     ! =0: dims are copied.

  INTEGER, INTENT(IN) :: icopy_lists ! =1 to require copying of all lists

  INTEGER, INTENT(IN) :: iwant_1deq  ! =1 to request fcns of (rho_eq)
                                     !  even if cclist(ps_cnum_EQ).eq.0

  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,ii,itot,isize,jsize,iertmp
  INTEGER :: ilock_mdescr,ilock_sconfig,ilock_siminit
  LOGICAL :: partial_copy
  !-------------------

  ierr = 0
  iout = SWIM_err_out_file

  IF((idata_only.EQ.1).or.(idims_only.EQ.1)) then
     partial_copy=.TRUE.
  ELSE
     partial_copy=.FALSE.
     DO ii=1,ps_ccount
        IF(ps_cc_all(ii).NE.cclist(ii)) THEN
           partial_copy=.TRUE.
        ENDIF
     ENDDO
  ENDIF
""")

    #-------------------------------
    # plasma_state_merge

    f_merge.write("""
SUBROUTINE plasma_state_merge(cclist,w1,ss1,ss2,ss3,icheck,ierr)

  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  ! cclist(...) control specifies the components for which data are merged.
  !   If cclist(j)=1 all data are merged for component j.
  !   If cclist(j)=0, data merger suppressed for this component.
  !   NOTE: species list data if present is checked, regardless of cclist(...).

  ! Non-floating-point data is copied from ss1
  ! with warnings if ss1 & ss2 values do not match & icheck = ps_warn
  ! with error messages and ierr set if values do not match & icheck = ps_error

  ! if an array is not allocated in (ss3):
  !    no action taken, regardless of allocation status in (ss1) or (ss2).

  ! if an array is allocated in (ss3) but in neither (ss1) nor (ss2):
  !    no action taken.

  ! if an array element is allocated in all of {(ss1),(ss2),(ss3)}:
  !    all must be conformal in shape; for grids, (ss1) & (ss2) values
  !    must match as well.
  !       for floating point in state_data or state_profile sections:
  !           ss3 <-- w1*ss1 + (1-w1)*ss2 formula ... is applied.
  !       for everything else: ss1 values copied to ss3.

  ! for quantities associated with species lists, values must match...

  ! if an array element is allocated in {(ss1),(ss3)}, not (ss2):
  !    (ss1) values are copied to (ss3); all arrays must be conformal in shape.

  ! if an array element is allocated in {(ss2),(ss3)}, not (ss1):
  !    (ss2) values are copied to (ss3); all arrays must be conformal in shape.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize
  INTEGER :: ilock_mdescr,ilock_sconfig,ilock_siminit,iertmp
  !-------------------

  iout = SWIM_err_out_file
  ierr = 0

""")

    #-------------------------------
    # plasma_state_prune_grids

    f_prune.write("""
SUBROUTINE plasma_state_prune_grids(cclist,ss,ierr)

  ! for each component deactivated in cclist(:), remove component grids
  ! and any profiles which use these grids

  ! if the EQ component is deactivated, remove equilibrium-derived quantities.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components
                                !   cclist(j)=0 => remove grids of component j

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,iertmp
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

  IF(cclist(ps_cnum_EQ).eq.0) call ps_purge_equilibrium(ss%iobj, iout, ierr)

""")

    #-------------------------------
    # plasma_state_hash_match

    f_ahash.write("""
LOGICAL FUNCTION plasma_state_hash_match(ss,iaddr,ierr)

  ! return .TRUE. if hash codes match, implying no change in data
  ! this routine assumes hash codes are up-to-date:
  ! ...CALL plasma_hash_state_update(ss,ierr) first!!!

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss       ! state data to check
  INTEGER, INTENT(IN) :: iaddr    ! address of hash codes to check
  INTEGER, INTENT(OUT) :: ierr    ! completion code (0=OK)

  ! non-zero ierr value indicates iaddr out of range

  !-------------------
  INTEGER :: iout
  !-------------------

  ierr = 0
  plasma_state_hash_match = .FALSE.

  iout = SWIM_err_out_file

  IF((iaddr.LE.0).or.(iaddr.GE.ps_hash_total)) THEN
     WRITE(iout,*) ' ?plasma_state_hash_match: iaddr out of expected range.'
     WRITE(iout,*) '  iaddr = ',iaddr,'; expected range: 1 to ',ps_hash_total-1
     ierr=9999
     RETURN
  ENDIF

  plasma_state_hash_match =(ss%hash_codes(iaddr).eq.ss%new_hash_codes(iaddr)) &
                   .AND.(ss%hash_codes(iaddr+1).eq.ss%new_hash_codes(iaddr+1))

END FUNCTION plasma_state_hash_match

SUBROUTINE plasma_state_hash_mark(ss,iaddr,ierr)

  ! mark indicated item as up-to-date, by copying its hash codes

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss       ! state data to check
  INTEGER, INTENT(IN) :: iaddr    ! address of hash codes to check
  INTEGER, INTENT(OUT) :: ierr    ! completion code (0=OK)

  ! non-zero ierr value indicates iaddr out of range

  !-------------------
  INTEGER :: iout
  !-------------------

  ierr = 0
  iout = SWIM_err_out_file

  IF((iaddr.LE.0).or.(iaddr.GE.ps_hash_total)) THEN
     WRITE(iout,*) ' ?plasma_state_hash_mark: iaddr out of expected range.'
     WRITE(iout,*) '  iaddr = ',iaddr,'; expected range: 1 to ',ps_hash_total-1
     ierr=9999
     RETURN
  ENDIF

  ss%hash_codes(iaddr) = ss%new_hash_codes(iaddr)
  ss%hash_codes(iaddr+1) = ss%new_hash_codes(iaddr+1)

END SUBROUTINE plasma_state_hash_mark

SUBROUTINE plasma_state_hash_addr(htype,hname,iaddr)

  ! return base hash address for named entity of indicated type
  !   iaddr -- base; iaddr+1 -- 2ndary hash address

  ! (at present this routine does not know about component-specific
  ! section hash codes)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  ! character string inputs are copied into local variables (C*32)
  ! which are converted to uppercase

  CHARACTER*(*), INTENT(IN) :: htype  ! hash entity type:
  !                             "SECTION" or "DIMENSION" or "PROFILE"

  CHARACTER*(*), INTENT(IN) :: hname  ! hash entity name:
  !                      example for "SECTION":  "MACHINE_DESCRIPTION"
  !                      example for "DIMENSION":  "RHO_EQ"
  !                      example for "PROFILE":  "TS"

  INTEGER, INTENT(OUT) :: iaddr       ! base hash address
  !  0 is returned & warning issued if htype & name do not map to a known
  !  quantity

  !----------------------------------
  CHARACTER*32 :: htypu,hnamu
  INTEGER :: iout
  !----------------------------------

  iout = SWIM_err_out_file

  !  fold input arguments to upper case

  htypu = htype
  hnamu = hname

  call uupper(htypu)
  call uupper(hnamu)

  iaddr = 0
""")

    f_hashd.write("""
SUBROUTINE plasma_state_hash_diff(ss1,lbl1,ss2,lbl2,sstr,icomp,idiff)

  ! summarize difference between two states, based on hash code comparison.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss1        ! 1st state
  CHARACTER*(*), intent(in) :: lbl1 ! label for 1st state

  TYPE (plasma_state) :: ss2        ! 2nd state
  CHARACTER*(*), intent(in) :: lbl2 ! label for 2nd state

  CHARACTER*(*), intent(in) :: sstr ! search string
  !  if section, dimension, or profile name contains sstr as a substring
  !  check the hash difference for that item.

  !  ==> if sstr = "*", check the hash difference for ALL items.

  INTEGER, intent(out) :: icomp     ! number of hash comparisons done
  INTEGER, intent(out) :: idiff     ! number of DIFFERENCES found

  !-------------------
  INTEGER :: iout,ils,iaddr1,iaddr2
  CHARACTER*32 ss
  !-------------------

  iout = SWIM_err_out_file

  ss = sstr
  call uupper(ss)
  ils = len(trim(ss))

  icomp = 0  ! number hash pairs compared
  idiff = 0  ! number that were different
""")

    #-------------------------------
    # plasma_state_hash_code

    f_hashc.write("""
SUBROUTINE plasma_state_hash_code(ss,cclist,ierr)

  ! update hash codes for state object ss: write to ss%new_hash_codes(...).
  !   hash codes for grids & profiles that are unallocated: ps_unInit
  !   hash codes for profiles that are all ZERO: -1
  ! otherwise hash codes are a pair positive integers, one evaluated by
  ! walking "forward" through the data; one by walking backwards through
  ! the data.
  !
  ! set cclist(j)=0 to suppress hash code update for all items owned
  !    by component (j) = ps_cnum_<component>.
  ! set cclist(j)=ps_chkProfs=2 to do hash code update only for grid and
  !    profile quantities owned by component (j) = ps_cnum_<component>.

  USE plasma_state_definition_mod
  USE plasma_hash_code
  IMPLICIT NONE

  TYPE (plasma_state) :: ss
  INTEGER, INTENT(IN) :: cclist(ps_ccount)
  INTEGER, INTENT(OUT) :: ierr    ! completion code (0=OK)

  ! if ierr is set, a change detected in a locked section
  ! in this case, messages are written as well.

  !-------------------
  INTEGER :: iout,ilock,iaddr1,iaddr2,isize,r8size,iertot,ii
  INTEGER :: kf1,kf2      ! working hash codes
  LOGICAL :: idiff,plasma_state_hash_match
  logical :: iprofs_only,icheck_any,icheck_all
  !-------------------

  ierr = 0
  iout = SWIM_err_out_file

  iertot = 0

  icheck_any=.FALSE.
  iprofs_only=.TRUE.
  icheck_all=.TRUE.

  ! check whether to skip scalar hash updates

  do ii=1,ps_ccount
     if(cclist(ii).ne.ps_chkAll) icheck_all=.FALSE.

     if(cclist(ii).ne.ps_nocheck) then
        icheck_any=.TRUE.
        if(cclist(ii).eq.ps_chkAll) then
           iprofs_only=.FALSE.
        else if(cclist(ii).eq.ps_chkProfs) then
           continue
        else
           ierr=1
        endif
     endif
  enddo

  if(ierr.eq.1) then
     write(iout,*) ' ?plasma_state_hash_code: illegal element value in'
     write(iout,*) '  CCLIST array: ',cclist
     write(iout,*) '  legal values are: ',ps_nocheck,ps_chkAll,ps_chkProfs
     write(iout,*) '    (ps_nocheck,ps_chkAll,ps_chkProfs).'
     return
  endif

  if(.NOT.icheck_any) return

  if(iprofs_only) go to 1000
""")

    #-------------------------------
    # plasma_state_empty_alloc

    f_alloc.write("""
SUBROUTINE plasma_state_empty_alloc(ss,ierr)

  ! incremental allocation
  ! allocate arrays not previously allocated -- only done at initialization.
  !   initialization steps:
  !      0.  declaration of state gives array dimensions values of zero.
  !      1.  set some array dimensions.
  !      2.  call this routine via PS_alloc_state(ierr); arrays with non-
  !          zero dimensioning are allocated.
  !          --ERROR-- if arrays were previously allocated with different
  !          dimension sizes.
  !      3.  based on further calculations or data access, set some more
  !          array dimensions; call this routine again to allocate all
  !          arrays associated with these additional, newly specified
  !          dimensions.  Do not change dimensions already set.  Repeat
  !          until entire structure is allocated and filled in.
  !
  ! It is an error to try to change the size of an already-allocated array.
  !

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state copy to receive empty allocation
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize,jsize
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0
""")

    #-------------------------------
    # plasma_state_erase

    f_erase.write("""
SUBROUTINE plasma_state_erase(ss,ierr)

  ! deallocate state object and reset all scalars to their initial values
  !   (empty state made identical to its initial instantiation)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state object whose elements are reinitialized
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize
  !-------------------

  iout = SWIM_err_out_file

  !  clear associated xplasma structures...
  call ps_init_xpl(ss%iobj,iout,ierr)  ! ierr return ignored.

  call plasma_state_free(ss,ierr)

  !  ss%itag, ss%chtag, ss%iobj not touched...

  ss%version_id = ps_version_id  ! this goes with ps code version

  ss%hash_codes = ps_unInit
  ss%new_hash_codes = ps_unInit
  ss%saved_hash_codes = ps_unInit

  ss%active_dims = 0
""")
    #-------------------------------
    # plasma_state_free

    f_free.write("""
SUBROUTINE plasma_state_free(ss,ierr)

  ! deallocate state object
  !   deallocate anything that was previously allocated.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state object whose elements are deallocated
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  ! errors -- expected array dimensions undefined
  !           arrays already allocated

  !-------------------
  INTEGER :: iout,isize
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

""")

    #-------------------------------
    # plasma_state_clear_profs

    f_clear.write("""
SUBROUTINE plasma_state_clear_profs(cclist,ss,ierr)

  !   set profile data to ZERO
  !   scalar data and id & name variables unaffected.
  !   grids and species lists and item lists unaffected.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components in which to clear profiles
                                ! use ps_cc_all to clear all components

  TYPE (plasma_state) :: ss     ! state object whose profiles are to be cleared
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  ! errors -- expected array dimensions undefined
  !           arrays already allocated

  !-------------------
  INTEGER :: iout,isize
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

""")

    #-------------------------------
    # plasma_state_zonwt_update

    f_zonwt.write("""
SUBROUTINE plasma_state_zonwt_update(ss,ierr)

  !   update zone weighting index map

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss    ! state object to be updated
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------
  INTEGER :: iout,isize,i1,i2,i3,i4,iertot,iertarg,ierval
  !-------------------

  iout = SWIM_err_out_file

  ierr = 0

  CALL ps_maxid(ss%iobj, isize, iout, ierr)
  if(ierr.NE.0) return

  ! reallocate to a safe size; initialize all elements to ZERO

  IF(ALLOCATED(ss%id_zonwt)) DEALLOCATE(ss%id_zonwt)
  ALLOCATE(ss%id_zonwt(isize))
  ss%id_zonwt = 0

  iertot = 0
""")

    #-------------------------------
    # info on dimensions

    f_pscon.write("""
SUBROUTINE ps_contents_alloc(itype,icomponent,w,ierr)

  !  get counts on various tables of contents

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (ps_list_window) :: w     ! list window structure allocated

  INTEGER, INTENT(OUT) :: ierr   ! status completion code (0 = OK)

  !local:

  INTEGER :: iout,isize

  !----------------
  iout = SWIM_err_out_file
  ierr = 0

  call free_window(w)

  isize = 0

  if(itype.eq.ps_grid_dims) then
     ! grid
     w%list_name='Grids'
  else if(itype.eq.ps_species_lists) then
     ! species list
     w%list_name='Species_Lists'
  else if(itype.eq.ps_item_lists) then
     ! item list
     w%list_name='Item_Lists'
  else if(itype.eq.ps_machine_descr) then
     ! machine description
     w%list_name='Machine_Description'
  else if(itype.eq.ps_shot_config) then
     ! shot configuration
     w%list_name='Shot_Configuration'
  else if(itype.eq.ps_sim_control) then
     ! simulation initialization data
     w%list_name='Simulation_Init'
  else if(itype.eq.ps_data) then
     ! time varying state data
     w%list_name='State_Data'
  else if(itype.eq.ps_profiles) then
     ! profile data
     w%list_name='Profiles'
  else if(itype.eq.ps_enum_dims) then
     ! enumeration dimensions
     w%list_name='Enumeration_Dimensions'
  else
     write(iout,'(a,i5,a)') '?ps_contents_alloc: itype = ',itype,' invalid.'
     ierr = 1
     return
  endif


""")

    f_pscon2.write("""
SUBROUTINE plasma_state_contents(itype,icomponent,ss,w,ierr)

  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(OUT) :: ierr   ! status code on exit, 0=normal

  !local:

  INTEGER :: iout,isize,id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------
  iout = SWIM_err_out_file
  ierr = 0

  isize = 0

""")

    #-------------------------------
    # namelist I/O

    f_wmd.write("""
subroutine ps_mdescr_namelist_write(filename,ss,ierr)

  ! from current contents of state, write machine description namelist

  USE plasma_state_definition_mod
  USE ps_namwr_mod
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout, ilun_namel
  INTEGER, DIMENSION(:), ALLOCATABLE :: isize_vec
  !-------------------------------

  iout = SWIM_err_out_file

  CALL find_io_unit(ilun_namel)

  OPEN(UNIT=ilun_namel,file=filename,status='unknown',iostat=ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?ps_mdescr_namelist_write: OPEN failure on file: '// &
          TRIM(filename)
     RETURN
  ENDIF

  WRITE(unit=ilun_namel,fmt='(" &mdescr")')
""")

    f_wsc.write("""
subroutine ps_sconfig_namelist_write(filename,ss,ierr)

  ! from current contents of state, write shot configuration namelist

  USE plasma_state_definition_mod
  USE ps_namwr_mod
  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  TYPE (plasma_state) :: ss     ! state containing data to write
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout, ilun_namel
  INTEGER, DIMENSION(:), ALLOCATABLE :: isize_vec
  !-------------------------------

  iout = SWIM_err_out_file

  CALL find_io_unit(ilun_namel)

  OPEN(UNIT=ilun_namel,file=filename,status='unknown',iostat=ierr)
  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?ps_sconfig_namelist_write: OPEN failure on file: '// &
          TRIM(filename)
     RETURN
  ENDIF

  WRITE(unit=ilun_namel,fmt='(" &sconfig")')
""")

    f_rmd.write("""
subroutine ps_mdescr_namelist_read(init,filename,tmp_filename,g_filename,ss, &
                                   ierr)

  ! read machine description namelist, copy into state
  ! if state is not empty, namelist input must not change existing
  ! non-zero array dimensions.

  USE plasma_state_definition_mod
  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: init                  ! .TRUE. to pre-initialize
  ! state variables prior to namelist read (caution -- could cause problems
  ! if partial machine description is already in the state).

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  CHARACTER*(*), INTENT(IN) :: tmp_filename    ! namelist (comments removed)
  !  (leave tmp_filename blank to get internally generated filename)
  CHARACTER*(*), INTENT(IN) :: g_filename      ! if non-blank: G-EQDSK filename

  ! if G-eqdsk filename is provided, limiter and bounding box in G-EQDSK
  ! file will override that given in the machine description namelist.;

  TYPE (plasma_state) :: ss     ! state containing data to read
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout,iertmp,isize,isiz0,ii
  INTEGER :: ilun_namel
  !-------------------------------
""")

    f_rsc.write("""
subroutine ps_sconfig_namelist_read(init,filename,tmp_filename,ss,ierr)

  ! from current contents of state, write shot configuration namelist

  USE plasma_state_mod
  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: init                  ! .TRUE. to pre-initialize
  ! state variables prior to namelist read (caution -- could cause problems
  ! if partial configuration dataset is already in the state).

  CHARACTER*(*), INTENT(IN) :: filename        ! file to contain namelist
  CHARACTER*(*), INTENT(IN) :: tmp_filename    ! namelist (comments removed)
  !  (leave tmp_filename blank to get internally generated filename)
  TYPE (plasma_state) :: ss     ! state containing data to read
  INTEGER, INTENT(OUT) :: ierr  ! completion code (0=OK)

  !-------------------------------
  INTEGER :: iout,iertmp,isize,isiz0,ii
  INTEGER :: ilun_namel
  !-------------------------------
""")

    #-------------------------------
    # index file

    doc_header_write(f_index)

    doc_header_write(f_cindx)

def doc_header_write(f):

    # write header info for generated index.txt files

    f.write("""
Python code generated description of PLASMA_STATE definition:
=============================================================

For the original specification and description of elements and parts of
the plasma state and its definition, please see:

                   plasma_state_spec.dat ...

For an alphabetic listing of all plasma state components and data elements,
please see:

                   plasma_state_index.txt ...

For a listing of data elements, sorted by component, and sorted within
each component by section, please see:

                   ps_component_index.txt ...

Every data element belongs to a physics component and to one of the
following section categories:

   MACHINE_DESCRIPTION  --  time invariant within a simulation.
   SHOT_CONFIGURATION   --  time invariant within a simulation.
   SIMULATION_INIT      --  time invariant within a simulation.

      data elements in the above are scalars or arrays of rank at most 1.

   STATE_DATA     --  time varying scalars and arrays of scalars.
   STATE_PROFILES --  time varying gridded profile data.

=============================================================
=============================================================

""")

#--------------------------------------------------------------------------
#  Now write the generated code...
#  (not including headers)

def plasma_state_write():

    plasma_state_write_index()
    plasma_state_write_cindx()

    plasma_state_write_constants()
    plasma_state_write_util()

    plasma_state_write_erase()
    plasma_state_write_free()
    plasma_state_write_clear()

    plasma_state_write_alloc()
    plasma_state_write_copy()

    plasma_state_write_def()

    plasma_state_write_calloc()

    plasma_state_write_contents()

    plasma_state_write_zonwt()

    plasma_state_write_prune()

    plasma_state_write_hash_code()
    plasma_state_write_hash_diff()

    plasma_state_write_get()
    plasma_state_write_store()
    plasma_state_write_xload()

    plasma_state_write_upwr()
    plasma_state_write_uprd()

    plasma_state_write_namio()

    plasma_state_write_pspcat()

#--------------------------------------------------------------------------
#  Constants...
#  Write the module containing the PS_ parameters from the "Constants"
#  section of the state specification...

def plasma_state_write_constants():

    print('...plasma_state_write_constants()...')

    #-----------------------------------------------
    # GENERATE CODE

    f_const.write("\n")
    f_const.write("  !---------------------------------------\n")
    f_const.write("  ! module parameters (constants):\n")
    f_const.write("\n")

    for item in all_constants_order:
        [ftype,rhs,cmts] = all_constants[item]
        name = item.lower()
        decl_str = "  " + ftype + ", PARAMETER, PUBLIC :: " + name + " = " + rhs
        f_const.write('\n' + decl_str + '\n')
        for cmt in cmts:
            f_const.write("         ! " + cmt + "\n")

    f_const.write("\n")
    f_const.write("  !---------------------------------------\n")
    f_const.write("  ! CCW parameters (constants):\n")
    f_const.write("\n")

    idecl = standard_dtype("I"," "," ")

    # DISABLE CCW constants for now... DMC interim Nov 2007

    for dimkey in all_dims:
        ddescr = all_items[dimkey]
        name = ddescr["name"]
        if "orientation" in ddescr:
            orientation = ddescr["orientation"]
            ccwnam = "ps_ccw_" + name
            decl_str = "  " + idecl + ", PARAMETER, PUBLIC :: " + ccwnam
            if orientation == "CCW":
                decl_str = decl_str + " = ps_ccw_select"
            else:
                decl_str = decl_str + " = ps_ccw_reverse"

            f_const.write(decl_str + '\n')

    f_const.write("\n")
    f_const.write("END MODULE plasma_state_constants_mod\n")

#--------------------------------------------------------------------------
#  Write text file index of plasma state contents...
#  This is an alphabetized index of the plasma state variables

def plasma_state_write_index():

    print('...plasma_state_write_index()...')

    f_index.write("""
SWIM components:

""")

    cclist = list(all_components.keys())
    cclist.sort()

    for cc in cclist:
        cmts = all_components[cc]

        f_index.write("  " + cc + ":\n")
        for cmt in cmts:
            f_index.write("     " + cmt + "\n")

    f_index.write("""
Alphabetic index of SWIM state variables.
""")

    ilist = list(all_items.keys())
    ilist.sort()

    for key in ilist:
        f_index.write('\n')
        f_index.write(key + ':\n')

        idescr = all_items[key]

        name = idescr['name']
        section = idescr['section']
        spec    = idescr['spec']
        component = idescr['component']
        if 'dlist' in idescr:
            dlist = idescr['dlist']
            itype = 'ARRAY'
        else:
            itype = 'SCALAR'

        ftype = idescr['ftype']
        dtype = idescr['dtype']
        if dtype == "R":
            units = idescr['units']

        cmts = idescr['cmts']

        f_index.write('  Section:  ' + section + '\n')
        f_index.write('  Specification:  ' + spec + '\n')

        if 'methkey' in idescr:
            methkey = idescr['methkey']
            if methkey == "Hermite_derivative":
                f_index.write('  Note: ' + methkey + '\n')

        if itype == "SCALAR":
            f_index.write('  Declaration:  ' + ftype + ' ps%' + name + '\n')
        else:
            dimstr = gen_dimstr('ps%',dlist)
            dstr = '  Declaration:  ' + ftype + ' ps%' + name + dimstr
            f_index.write(dstr + '\n')

        if dtype == "R":
            f_index.write('  Units:  ' + units + '\n')

        f_index.write('  Component:  ' + component + '\n')
        f_index.write('  Comments:\n')
        for cmt in cmts:
            f_index.write('     ' + cmt + '\n')

#--------------------------------------------------------------------------
#  Write text file index of plasma state contents...
#  This is an alphabetized index of the plasma state variables

def plasma_state_write_cindx():

    print('...plasma_state_write_cindx()...')

    loc_sects = []
    loc_sects.append("MACHINE_DESCRIPTION")
    loc_sects.append("SHOT_CONFIGURATION")
    loc_sects.append("SIMULATION_INIT")
    loc_sects.append("STATE_DATA")
    loc_sects.append("STATE_PROFILES")

    #  check that all sections are in loc_sects list; warn if any are missing.

    ilist = list(all_items.keys())
    ilist.sort()

    cdict = {}

    for key in ilist:
        idescr = all_items[key]
        section = idescr['section']
        if section not in loc_sects:
            print(" ")
            print("NOTE: plasma_state_write_cindx: element " + key)
            print("contains unknown SECTION id (appended to list): " + section)
            loc_sects.append(section)

        component = idescr['component']
        if component not in list(cdict.keys()):
            cdict[component] = []
        cdict[component].append(key)

    f_cindx.write("""
Data elements, sorted by SWIM physics components (components listed in
alphabetic order; elements sorted by section within each component listing):

""")

    cclist = list(all_components.keys())
    cclist.sort()

    for cc in cclist:
        cmts = all_components[cc]

        f_cindx.write("\n")
        f_cindx.write("-------------------------------------------------------------------------\n")
        f_cindx.write("Physics component:  " + cc + ":\n")
        for cmt in cmts:
            f_cindx.write("     " + cmt + "\n")

        for sect in loc_sects:
            f_cindx.write("\n")
            f_cindx.write("  Section: " + sect + " (component: " + cc + ")\n")

            ict = 0
            for elem in cdict[cc]:
                idescr = all_items[elem]
                section = idescr['section']

                if section == sect:
                    f_cindx.write("\n")
                    f_cindx.write("    " + elem + ":\n")
                    ict = ict + 1

                    name = idescr['name']
                    spec    = idescr['spec']
                    if 'dlist' in idescr:
                        dlist = idescr['dlist']
                        itype = 'ARRAY'
                    else:
                        itype = 'SCALAR'

                    ftype = idescr['ftype']
                    dtype = idescr['dtype']
                    if dtype == "R":
                        units = idescr['units']

                    cmts = idescr['cmts']

                    f_cindx.write('      Specification:  ' + spec + '\n')

                    if 'methkey' in idescr:
                        methkey = idescr['methkey']
                        if methkey == "Hermite_derivative":
                            f_cindx.write('      Note: ' + methkey + '\n')

                    if itype == "SCALAR":
                        f_cindx.write(
                            '      Declaration:  ' + ftype +
                            ' ps%' + name + '\n')
                    else:
                        dimstr = gen_dimstr('ps%',dlist)
                        dstr = '      Declaration:  ' + ftype + ' ps%' + name + dimstr
                        f_cindx.write(dstr + '\n')

                    if dtype == "R":
                        f_cindx.write('      Units:  ' + units + '\n')

                    f_cindx.write('      Comments:\n')
                    for cmt in cmts:
                        f_cindx.write('        ' + cmt + '\n')

            if ict == 0:
                f_cindx.write("    (no data elements)\n")


#--------------------------------------------------------------------------
#  Write utility module -- generated parameter sets giving component
#  names and descriptions and some basic statistics derived from the
#  specification...

def plasma_state_write_util():

    print('...plasma_state_write_util()...')

    cclist = list(all_components.keys())
    cclist.sort()

    inum = len(cclist)

    #---------------------------------------
    # component list encoded in PARAMETERs

    f_util.write("\n")
    f_util.write("  ! component information\n")
    f_util.write("\n")
    f_util.write("  INTEGER, PARAMETER, PUBLIC :: ps_ccount = " + repr(inum) +
                 "  ! number of known components\n")

    f_util.write("\n")
    f_util.write("  ! C*32 parameter name for each component:\n")

    comp_ndims = []

    for cname in cclist:
        cmts = all_components[cname]
        cdescr = cmts[0]

        f_util.write("  CHARACTER*32, PARAMETER, PUBLIC :: ps_cname_" +
                     cname + " = '" + cname + "'\n")
        f_util.write("  CHARACTER*80, PARAMETER, PUBLIC :: ps_cdescr_" +
                     cname + " = &\n")
        f_util.write("     '" + cdescr + "'\n")

        comp_ndims.append(0)

    # compute number of dimensions in each component

    indx = -1
    for cname in cclist:
        indx = indx + 1
        for dimkey in all_dims:
            idescr = all_items[dimkey]
            cur_component = idescr['component']
            if cur_component == cname:
                comp_ndims[indx] = comp_ndims[indx] + 1

    f_util.write("\n")
    f_util.write("  ! INTEGER parameter number for each component:\n")

    cczero = "(/ "
    cc_all = "(/ "
    cc_ndims = "(/ "
    icount = 0
    for cname in cclist:
        indx = icount
        icount = icount + 1
        f_util.write("  INTEGER, PARAMETER, PUBLIC :: ps_cnum_" +
                     cname + " = " + repr(icount) + "\n")
        if icount < inum:
            cczero = cczero + "0,"
            cc_all = cc_all + "1,"
            cc_ndims = cc_ndims + repr(comp_ndims[indx]) + ","
        else:
            cczero = cczero + "0 /)"
            cc_all = cc_all + "1 /)"
            cc_ndims = cc_ndims + repr(comp_ndims[indx]) + " /)"

    f_util.write("\n")
    f_util.write("  ! array of names for each component:\n")
    f_util.write("  CHARACTER*32, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cnames = (/ &\n")

    icount = 0
    for cname in cclist:
        icount = icount + 1
        if icount < inum:
            trl = ", &\n"
        else:
            trl = " /)\n"
        f_util.write("       ps_cname_" + cname + trl)

    f_util.write("\n")
    f_util.write("  ! array of descriptions for each component:\n")
    f_util.write("  CHARACTER*80, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cdescrs = (/ &\n")

    icount = 0
    for cname in cclist:
        icount = icount + 1
        if icount < inum:
            trl = ", &\n"
        else:
            trl = " /)\n"
        f_util.write("       ps_cdescr_" + cname + trl)

    f_util.write("\n")
    f_util.write("  INTEGER, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cczero = &\n")
    f_util.write("       " + cczero + "\n")

    f_util.write("\n")
    f_util.write("  INTEGER, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_cc_all = &\n")
    f_util.write("       " + cc_all + "\n")

    f_util.write('\n')
    f_util.write("  INTEGER, PARAMETER, DIMENSION(ps_ccount), PUBLIC :: ps_ndims = &\n")
    f_util.write("       " + cc_ndims + "\n")

    #---------------------------------------
    # coordinate list encoded in PARAMETERs

    f_util.write("\n")
    f_util.write("  ! coordinate information\n")

    coord_list = list(all_coords.keys())
    coord_list.sort()

    coord_num = len(coord_list)

    f_util.write('\n')
    f_util.write('  INTEGER, PARAMETER :: ps_coordCount = ' + repr(coord_num) + '\n')

    onval = "(/ "
    offval = "(/ "

    f_util.write('\n')
    icoord = 0
    for coord in coord_list:
        icoord = icoord + 1
        f_util.write('  INTEGER, PARAMETER :: ps_coordNum_' + coord +
                     ' = ' + repr(icoord) + '\n')
        if icoord < coord_num:
            onval = onval + "1,"
            offval = offval + "0,"
        else:
            onval = onval + "1 /)"
            offval = offval + "0 /)"

    f_util.write('\n')
    icoord = 0
    for coord in coord_list:
        icoord = icoord + 1
        f_util.write('  CHARACTER*32, PARAMETER :: ps_coordName_' + coord +
                     ' = "' + coord + '"\n')

    f_util.write('\n')
    f_util.write('  INTEGER, PARAMETER, DIMENSION(ps_coordCount) :: ' +
                 'ps_allCoords = ' + onval + '\n')
    f_util.write('  INTEGER, PARAMETER, DIMENSION(ps_coordCount) :: ' +
                 'ps_noCoords = ' + offval + '\n')
    f_util.write('\n')
    f_util.write('  CHARACTER*32, PARAMETER, DIMENSION(ps_coordCount) :: ' +
                 'ps_coordNames = (/ &\n')

    icoord = 0
    for coord in coord_list:
        icoord = icoord + 1
        if icoord < coord_num:
            f_util.write('       ps_coordName_' + coord + ', &\n')
        else:
            f_util.write('       ps_coordName_' + coord + ' /)\n')

    #---------------------------------------
    # hash pointers

    iadr = -1

    f_util.write('\n')
    f_util.write('  ! hash addresses\n')

    f_util.write('\n')
    for section in all_sections:
        iadr = iadr + 2
        sname = section.lower()
        f_util.write("\n")
        f_util.write("  INTEGER, PARAMETER, PUBLIC ::"
                     + " ps_shash_" + sname + " = " + repr(iadr) + "\n")

        if section == "MACHINE_DESCRIPTION":
            mylist = mdescr
        elif section == "SHOT_CONFIGURATION":
            mylist = sconfig
        elif section == "SIMULATION_INIT":
            mylist = simcon
        elif section == "STATE_DATA":
            mylist = state_data_list
        elif section == "STATE_PROFILES":
            mylist = []
        else:
            print(" ?plasma_state_write (f_util): unexpected section: ",section)
            raise plasma_state_write_error

        f_util.write("\n")
        for item in mylist:
            if item not in all_dims:
                sdescr = all_items[item]
                name = sdescr['name']
                iadr = iadr + 2
                f_util.write("  INTEGER, PARAMETER, PUBLIC ::"
                             + " ps_shash_" + name + " = " + repr(iadr) + "\n")

    f_util.write('\n')
    for dim in all_dims:
        iadr = iadr + 2
        dname = dim.lower()
        f_util.write("  INTEGER, PARAMETER, PUBLIC ::"
                     + " ps_dhash_" + dname + " = " + repr(iadr) + "\n")

    f_util.write('\n')
    for prof in all_profiles:
        iadr = iadr + 2
        pname = prof.lower()
        f_util.write("  INTEGER, PARAMETER, PUBLIC ::"
                     + " ps_phash_" + pname + " = " + repr(iadr) + "\n")

    f_util.write('\n')
    f_util.write("  ! auxilliary hash code pair addresses:\n")
    icount = 0
    while icount < num_aux_hash_pairs:
        iadr = iadr + 2
        icount = icount + 1
        hashadr = "ps_hash_aux" + repr(icount)
        f_util.write("  INTEGER, PARAMETER, PUBLIC :: " + hashadr
                     + " = " + repr(iadr) + "\n")

    iadr = iadr + 1
    f_util.write('\n')
    f_util.write("  ! total number of hash codes:\n")
    f_util.write("  INTEGER, PARAMETER, PUBLIC :: ps_hash_total = " + repr(iadr) + "\n")

    f_util.write("""

contains

   ! routines to check for differences in values
   ! set iflag if differences are detected AND policy = ps_error

   subroutine ps_diff_c0(name,c0a,c0b,policy,iout,iflag)
     character*(*), intent(in) :: name
     character*(*), intent(in) :: c0a
     character*(*), intent(in) :: c0b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     if(c0a.ne.c0b) then

        msghd = '%'
        if(policy.eq.ps_error) then
           iflag = 1
           msghd = '?'
        endif

        write(iout,*) msghd//'ps_diff: value difference detected in: '// &
             trim(name)
     endif
   end subroutine ps_diff_c0

   subroutine ps_diff_c1(name,c1a,c1b,policy,iout,iflag)
     character*(*), intent(in) :: name
     character*(*), dimension(:), intent(in) :: c1a
     character*(*), dimension(:), intent(in) :: c1b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(len(c1a(1)).ne.len(c1b(1))) idiff=2
     if(size(c1a).ne.size(c1b)) idiff=2
     if(idiff.eq.0) then
        do ii=1,size(c1a)
           if(c1a(ii).ne.c1b(ii)) then
              idiff=1
              exit
           endif
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_c1

   subroutine ps_diff_i0(name,i0a,i0b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, intent(in) :: i0a
     integer, intent(in) :: i0b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     if(i0a.ne.i0b) then

        msghd = '%'
        if(policy.eq.ps_error) then
           iflag = 1
           msghd = '?'
        endif

        write(iout,*) msghd//'ps_diff: value difference detected in: '// &
             trim(name)
     endif
   end subroutine ps_diff_i0

   subroutine ps_diff_i1(name,i1a,i1b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:), intent(in) :: i1a
     integer, dimension(:), intent(in) :: i1b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(i1a).ne.size(i1b)) idiff=2
     if(idiff.eq.0) then
        do ii=1,size(i1a)
           if(i1a(ii).ne.i1b(ii)) then
              idiff=1
              exit
           endif
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_i1

   subroutine ps_diff_i2(name,i2a,i2b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:), intent(in) :: i2a
     integer, dimension(:,:), intent(in) :: i2b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(i2a,1).ne.size(i2b,1)) idiff=2
     if(size(i2a,2).ne.size(i2b,2)) idiff=2
     if(idiff.eq.0) then
        do jj=1,size(i2a,2)
           do ii=1,size(i2a,1)
              if(i2a(ii,jj).ne.i2b(ii,jj)) then
                 idiff=1
                 exit
              endif
           enddo
           if(idiff.ne.0) exit
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_i2

   subroutine ps_diff_i3(name,i3a,i3b,policy,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:,:), intent(in) :: i3a
     integer, dimension(:,:,:), intent(in) :: i3b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(i3a,1).ne.size(i3b,1)) idiff=2
     if(size(i3a,2).ne.size(i3b,2)) idiff=2
     if(size(i3a,3).ne.size(i3b,3)) idiff=2
     if(idiff.eq.0) then
        do kk=1,size(i3a,3)
           do jj=1,size(i3a,2)
              do ii=1,size(i3a,1)
                 if(i3a(ii,jj,kk).ne.i3b(ii,jj,kk)) then
                    idiff=1
                    exit
                 endif
              enddo
              if(idiff.ne.0) exit
           enddo
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_i3

   subroutine ps_diff_r0(name,r0a,r0b,policy,iout,iflag)
     character*(*), intent(in) :: name
     real*8, intent(in) :: r0a
     real*8, intent(in) :: r0b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     if(r0a.ne.r0b) then

        msghd = '%'
        if(policy.eq.ps_error) then
           iflag = 1
           msghd = '?'
        endif

        write(iout,*) msghd//'ps_diff: value difference detected in: '// &
             trim(name)
     endif
   end subroutine ps_diff_r0

   subroutine ps_diff_r1(name,r1a,r1b,policy,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:), intent(in) :: r1a
     real*8, dimension(:), intent(in) :: r1b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(r1a).ne.size(r1b)) idiff=2
     if(idiff.eq.0) then
        do ii=1,size(r1a)
           if(r1a(ii).ne.r1b(ii)) then
              idiff=1
              exit
           endif
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_r1

   subroutine ps_diff_r2(name,r2a,r2b,policy,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:), intent(in) :: r2a
     real*8, dimension(:,:), intent(in) :: r2b
     integer, intent(in) :: policy
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------
     if(policy.eq.ps_ignore) return

     idiff=0
     if(size(r2a,1).ne.size(r2b,1)) idiff=2
     if(size(r2a,2).ne.size(r2b,2)) idiff=2
     if(idiff.eq.0) then
        do jj=1,size(r2a,2)
           do ii=1,size(r2a,1)
              if(r2a(ii,jj).ne.r2b(ii,jj)) then
                 idiff=1
                 exit
              endif
           enddo
           if(idiff.ne.0) exit
        enddo
     endif

     if(idiff.ne.0) then

        msghd = '%'
        if((policy.eq.ps_error).OR.(idiff.eq.2)) then
           iflag = 1
           msghd = '?'
        endif

        if(idiff.eq.1) then
           write(iout,*) msghd//'ps_diff: value difference detected in: '// &
                trim(name)
        else
           write(iout,*) msghd//'ps_diff: shape difference detected in: '// &
                trim(name)
        endif
     endif
   end subroutine ps_diff_r2

   ! routines to check for conformal dimensioning of arrays
   ! set iflag and issue message if array dimensions do not match

   subroutine ps_conformal_c1(name,c1a,c1b,iout,iflag)
     character*(*), intent(in) :: name
     character*(*), dimension(:), intent(in) :: c1a
     character*(*), dimension(:), intent(in) :: c1b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(len(c1a(1)).ne.len(c1b(1))) idiff=2
     if(size(c1a).ne.size(c1b)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_c1

   subroutine ps_conformal_i1(name,i1a,i1b,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:), intent(in) :: i1a
     integer, dimension(:), intent(in) :: i1b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(i1a).ne.size(i1b)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_i1

   subroutine ps_conformal_i2(name,i2a,i2b,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:), intent(in) :: i2a
     integer, dimension(:,:), intent(in) :: i2b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(i2a,1).ne.size(i2b,1)) idiff=2
     if(size(i2a,2).ne.size(i2b,2)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_i2

   subroutine ps_conformal_i3(name,i3a,i3b,iout,iflag)
     character*(*), intent(in) :: name
     integer, dimension(:,:,:), intent(in) :: i3a
     integer, dimension(:,:,:), intent(in) :: i3b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(i3a,1).ne.size(i3b,1)) idiff=2
     if(size(i3a,2).ne.size(i3b,2)) idiff=2
     if(size(i3a,3).ne.size(i3b,3)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_i3

   subroutine ps_conformal_r1(name,r1a,r1b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:), intent(in) :: r1a
     real*8, dimension(:), intent(in) :: r1b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r1a).ne.size(r1b)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r1

   subroutine ps_conformal_r2(name,r2a,r2b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:), intent(in) :: r2a
     real*8, dimension(:,:), intent(in) :: r2b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r2a,1).ne.size(r2b,1)) idiff=2
     if(size(r2a,2).ne.size(r2b,2)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r2

   subroutine ps_conformal_r3(name,r3a,r3b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:,:), intent(in) :: r3a
     real*8, dimension(:,:,:), intent(in) :: r3b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r3a,1).ne.size(r3b,1)) idiff=2
     if(size(r3a,2).ne.size(r3b,2)) idiff=2
     if(size(r3a,3).ne.size(r3b,3)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r3

   subroutine ps_conformal_r4(name,r4a,r4b,iout,iflag)
     character*(*), intent(in) :: name
     real*8, dimension(:,:,:,:), intent(in) :: r4a
     real*8, dimension(:,:,:,:), intent(in) :: r4b
     integer, intent(in) :: iout
     integer, intent(inout) :: iflag

     integer :: ii,jj,kk,idiff

     character*1 msghd

     !--------------

     idiff=0
     if(size(r4a,1).ne.size(r4b,1)) idiff=2
     if(size(r4a,2).ne.size(r4b,2)) idiff=2
     if(size(r4a,3).ne.size(r4b,3)) idiff=2
     if(size(r4a,4).ne.size(r4b,4)) idiff=2

     if(idiff.ne.0) then

        msghd = '?'
        iflag = 1
        write(iout,*) msghd//'ps_conformal: shape difference detected in: '// &
                trim(name)

     endif
   end subroutine ps_conformal_r4

end module plasma_state_util_mod
""")

#------------------------------------------
#  write subroutine to erase data associated with plasma state

def plasma_state_write_erase():

    print('...plasma_state_write_erase()...')

    iilist = list(all_items.keys())
    iilist.sort()

    for key in iilist:
        idescr = all_items[key]

        name = idescr['name']

        idstat = id_status(idescr)
        if idstat == "STANDALONE":
            # scalar ID variable: clear it

            f_erase.write("\n")
            f_erase.write("  ss%id_" + name + " = 0\n")

        if 'dlist' not in idescr:
            # pure scalar: reset it

            init = idescr['init']

            f_erase.write("\n")
            f_erase.write("  ss%" + name + " = " + init + "\n")

    f_erase.write("""

END SUBROUTINE plasma_state_erase
""")

#------------------------------------------
#  write subroutine to free memory associated with plasma state

def plasma_state_write_free():

    print('...plasma_state_write_free()...')

    iilist = list(all_items.keys())
    iilist.sort()

    dealloc_write("id_zonwt")
    f_free.write("\n")

    for key in iilist:
        idescr = all_items[key]

        if 'dlist' in idescr:
            # it is an array...
            dlist = idescr['dlist']
            name = idescr['name']

            f_free.write("\n")

            idstat = id_status(idescr)
            if idstat == "ARRAY":
                # it is a profile with id array also allocatable
                idname = "id_" + name
                dealloc_write(idname)

            # deallocate the array itself:
            dealloc_write(name)

    f_free.write("""

END SUBROUTINE plasma_state_free
""")

def dealloc_write(name):

    # write deallocate statement for on f_free

    str = "  IF(ALLOCATED(ss%" + name + ")) DEALLOCATE(ss%" + name + ")\n"

    f_free.write(str)

#------------------------------------------
#  write subroutine to allocate memory associated with plasma state
#  only allocate arrays for which all dimensions are defined, i.e. have
#  values > 0:

def plasma_state_write_alloc():

    print("...plasma_state_write_alloc()...")

    f_alloc.write('  !==============================\n')
    f_alloc.write('  ! update enumeration dimension maxima\n')

    chk_enum_max(f_alloc,'ss%')

    f_alloc.write('  !==============================\n')
    f_alloc.write('  ! count dimensions with non-zero values (by component)\n')

    wr_count_dims(f_alloc)

    f_alloc.write('\n')
    f_alloc.write('  !==============================\n')
    f_alloc.write('  ! call routines to allocate arrays with non-zero dimensions\n')
    #  complete plasma_state_merge main routine
    #    (add calls to plasma_state_merge_<nnn> subroutines)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_empty_alloc_" + repr(isub_count)
        ff = f_alla[isub_count]

        arglist = "(ss,iout,ierr)\n"
        outcall = "  call " + sname + arglist
        f_alloc.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)
        ff.write("""

  ! Allocate array data for which all dimensions are provided

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize,jsize
  !-------------------

""")

        isub_count = isub_count + 1

    # end of sub-subroutine loop
    # complete the main routine...

    f_alloc.write("""

END SUBROUTINE plasma_state_empty_alloc
""")

    iilist = list(all_items.keys())
    iilist.sort()

    indent = []
    indent.append("  ")
    indent.append("     ")
    indent.append("        ")

    # list of (nsplit_sce) sublists
    isublists = sublist_gen(iilist)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ## print(" **** ",isub_count, isublists[isub_count])
        ff = f_alla[isub_count]

        ff.write('\n')
        ff.write("  !-----------------------------------------------\n")
        ff.write("  ! allocate arrays...\n")

        for key in isublists[isub_count]:
            idescr = all_items[key]
            if 'dlist' in idescr:

                # item is an array

                dlist = idescr['dlist']
                name = idescr['name']
                init = idescr['init']

                if 'rank_coord' in idescr:
                    # item is a profile
                    rank_coord = idescr['rank_coord']
                else:
                    rank_coord = 0

                ilev = 0

                mlines = []
                for dpair in dlist:
                    ilev = ilev + 1
                    dlower = dpair[0]
                    dupper = dpair[1]

                    if ilev == 1:
                        str2 = indent[0] + "isize = ("
                    else:
                        str2 = str2 + "*("

                    incr = gen_size_adjust(dlower)

                    str2 = str2 + "ss%" + dupper + incr + ")"

                    # zero out total size if upper dimension is zero, even when
                    # lower dimension is not 1.

                    if incr != "":
                        str1 = indent[0] + "IF(ss%" + dupper + ".LE.0) isize = 0\n"
                        mlines.append(str1)

                str2 = str2 + "\n"

                ff.write('\n')
                ff.write(str2)
                for mline in mlines:
                    ff.write(mline)

                ff.write(indent[0] + "IF(ALLOCATED(ss%" + name + ").AND.(isize.EQ.0)) then\n")
                ff.write(indent[1] +    "DEALLOCATE(ss%" + name + ")\n")
                ff.write(indent[0] + "ENDIF\n")

                ff.write(indent[0] + "IF(ALLOCATED(ss%" + name + ")) then\n")
                ff.write(indent[1] +    "jsize = SIZE(ss%" + name + ")\n")
                ff.write(indent[1] +    "IF(isize.NE.jsize) THEN\n")
                ff.write(indent[2] +       "WRITE(iout,*) &\n")
                str = '" ?plasma_state_empty_alloc: ' + name
                str = str + ' already allocated with size:"\n'
                ff.write(indent[2] +       "     " + str)
                str = 'WRITE(iout,*) "    old size: ",jsize\n'
                ff.write(indent[2] +       str)
                str = 'WRITE(iout,*) "    requested new size: ",isize\n'
                ff.write(indent[2] +       str)
                ff.write(indent[2] +       "ierr=999\n")
                ff.write(indent[1] +    "ENDIF\n")
                ff.write(indent[0] + "ELSE IF (isize.GT.0) THEN\n")

                ssize1 = gen_dimstr('ss%',dlist)
                str = "ALLOCATE(ss%" + name + ssize1 + ")\n"
                ff.write(indent[1] +    str)
                ff.write(indent[1] +    "ss%" + name + " = " + init + "\n")

                idstat = id_status(idescr)
                if idstat == "ARRAY":
                    ssize2 = gen_dimstr('ss%',dlist[rank_coord:])
                    str = "ALLOCATE(ss%id_" + name + ssize2 + ")\n"
                    ff.write(indent[1] +    str)
                    ff.write(indent[1] +    "ss%id_" + name + " = 0\n")

                ff.write(indent[0] + "ENDIF\n")

        isub_count = isub_count + 1

    # OK, final loop to write END statements

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ff = f_alla[isub_count]
        sname = "plasma_state_empty_alloc_" + repr(isub_count)

        ff.write("\nEND SUBROUTINE " + sname + "\n")
        isub_count = isub_count + 1

def gen_size_adjust(dlower):

    # adjust size based on dimension lower limit

    if dlower == "1" or dlower == "I" or dlower == "NEW":
        incr = ""
    elif dlower == "0":
        incr = "+1"
    elif dlower == "-1":
        incr = "+2"
    else:
        incr = "-" + dlower + "+1"

    return incr

#------------------------------------------
#  write subroutine to clear all profiles to zero.  IDs and non-profile
#  data unaffected.

def plasma_state_write_clear():

    print("...plasma_state_write_clear()...")

    f_clear.write("\n")
    for key in all_profiles:
        descr = all_items[key]
        name = descr['name']
        cur_component = descr['component']

        str = "  IF(cclist(ps_cnum_" + cur_component + ").EQ.1) THEN\n"
        f_clear.write(str)

        str = "     IF(ALLOCATED(ss%" + name + ")) ss%" + name
        str = str + " = 0.0_rspec\n"
        f_clear.write(str)

        f_clear.write("  ENDIF\n")

    f_clear.write("""

END SUBROUTINE plasma_state_clear_profs
""")

#------------------------------------------
#  gen code to check enum max dimensions

def chk_enum_max(f,prefix):

    for dimkey in all_dims:
        idescr = all_items[dimkey]
        name = idescr['name']
        dimtype = idescr['dimtype']
        if dimtype == "MAX":
            # name with "max_" removed:
            mname = name[4:]
            f.write("\n")
            f.write("  IF(ALLOCATED(" + prefix + mname + ")) THEN\n")
            f.write("     " + prefix + name + " = max(" + prefix + name +
                    ",maxval(" + prefix + mname + "))\n")
            f.write("  ENDIF\n")

    f.write("\n")

#------------------------------------------
#  write subroutine to copy plasma state contents between objects

def plasma_state_write_copy():

    print("...plasma_state_write_copy()...")

    f_xcopy.write("\n")
    f_xcopy.write("  !---------------------------------------------------\n")
    f_xcopy.write("  ! copy array dimensions (grids for indicated components only...)\n")
    f_xcopy.write("\n")

    chk_enum_max(f_xcopy,'ss%')

    for dimkey in all_dims:
        idescr = all_items[dimkey]
        name = idescr['name']
        cur_component = idescr['component']
        dimtype = idescr['dimtype']

        if dimtype in all_coords:
            if name == "nrho_eq":
                f_xcopy.write("  IF((cclist(ps_cnum_" + cur_component +
                              ").EQ.1).OR.(IWANT_1dEQ.eq.1)) &\n")
            else:
                f_xcopy.write("  IF(cclist(ps_cnum_" + cur_component +
                              ").EQ.1) &\n")
            f_xcopy.write('           call copydim("' + name + '", ss%' +
                          name + ',ss2%' + name + ')\n')

        elif dimkey not in species_list_attrib:
            f_xcopy.write("  IF((icopy_lists.EQ.1).OR.(cclist(ps_cnum_" +
                          cur_component + ").EQ.1)) &\n")
            f_xcopy.write('           call copydim("' + name + '", ss%' +
                          name + ',ss2%' + name + ')\n')
        else:
            dkey = name.upper()
            ddescr = all_items[dkey]
            dimlabel = ddescr['dimlabel']
            f_xcopy.write('  call copydim("' + name + '", ss%' +
                          name + ',ss2%' + name + ')\n')

    f_xcopy.write('\n')
    f_xcopy.write('  IF(ierr.NE.0) return\n')
    f_xcopy.write('  IF(idims_only.EQ.1) return\n')
    f_xcopy.write('\n')
    f_xcopy.write("  !---------------------------------------------------\n")
    f_xcopy.write("  ! allocate ss2 arrays as allowed by non-zero dimensions\n")
    f_xcopy.write('\n')
    f_xcopy.write('  call plasma_state_empty_alloc(ss2,ierr)\n')
    f_xcopy.write('  IF(ierr.NE.0) RETURN\n')

    f_xcopy.write('\n')
    f_xcopy.write("  !---------------------------------------------------\n")
    f_xcopy.write("  ! copy scalars and arrays that are allocated...\n")
    f_xcopy.write("  ! (but not array dimensions)\n\n")

    #  complete plasma_state_merge main routine
    #    (add calls to plasma_state_merge_<nnn> subroutines)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_merge_" + repr(isub_count)
        ff = f_merga[isub_count]

        arglist = "(cclist,w1,ss1,ss2,ss3,icheck,iout,ierr)\n"
        outcall = "  call " + sname + arglist
        f_merge.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)
        ff.write("""

  ! Merge floating point data
  !   ss3 <-- w1*ss1 + (1-w1)*ss2

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  REAL(KIND=rspec) :: w1        ! weight on ss1
  TYPE (plasma_state) :: ss1    ! state data to copy *from* with weight (w1)
  TYPE (plasma_state) :: ss2    ! state data to copy *from* with weight (1-w1)
  TYPE (plasma_state) :: ss3    ! state to which data is copied.
  INTEGER, INTENT(IN) :: icheck ! control for checking match of non-floating-
  !  point data: ps_ignore, ps_warn, or ps_error on mismatch.

  INTEGER, INTENT(IN) :: iout
  INTEGER, INTENT(INOUT) :: ierr  ! completion code (not cleared,set on error)

  !-------------------
  INTEGER :: isize
  !-------------------
""")

        isub_count = isub_count + 1

# end of sub-subroutine loop

    f_merge.write("""
  ! set hash codes of new copy of state

  ilock_mdescr = ss3%lock_machine_descr
  ilock_sconfig = ss3%lock_shot_config
  ilock_siminit = ss3%lock_sim_init

  ss3%lock_machine_descr = ps_unlocked
  ss3%lock_shot_config = ps_unlocked
  ss3%lock_sim_init = ps_unlocked

  CALL plasma_state_hash_code(ss3,ps_cc_all,iertmp)
  ss3%hash_codes = ss3%new_hash_codes

  ss3%lock_machine_descr = ilock_mdescr
  ss3%lock_shot_config = ilock_sconfig
  ss3%lock_sim_init = ilock_siminit

END SUBROUTINE plasma_state_merge
""")

    # plasma_state_merge.f90 completed

    iilist = list(all_items.keys())
    iilist.sort()

    # list of (nsplit_sce) sublists
    isublists = sublist_gen(iilist)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ## print(" **** ",isub_count, isublists[isub_count])
        ff = f_merga[isub_count]

        ff.write('\n')
        ff.write("  !-----------------------------------------------\n")
        ff.write("  ! copy scalars and arrays that are allocated...\n")

        for key in isublists[isub_count]:
            descr = all_items[key]
            name = descr['name']
            dtype = descr['dtype']
            section = descr['section']
            cur_component = descr['component']
            if 'dlist' in descr:
                dlist = descr['dlist']
                #  array
                ff.write('\n')
                if key in species_list_attrib:
                    #  list dimension info must be copied regardless of cclist
                    ff.write("  IF(ALLOCATED(ss3%" + name + ")) THEN\n")
                else:
                    ff.write("  IF((ALLOCATED(ss3%" + name +
                             ")).AND.(cclist(ps_cnum_" + cur_component +
                             ").EQ.1)) THEN\n")
                ff.write("     IF(ALLOCATED(ss1%" + name +
                         ").AND.ALLOCATED(ss2%" + name + ")) THEN\n")

                if key in grid_items or key in species_list_attrib:
                    ff.write('        CALL ps_diff("' + name +
                             '",ss1%' + name + ',ss2%' + name +
                             ',ps_error,iout,ierr)\n')
                    ff.write('        CALL ps_conformal("' + name +
                             '",ss1%' + name + ',ss3%' + name +
                             ',iout,ierr)\n')
                    ff.write("        if(ierr.eq.0) ss3%" + name + " = ss1%" + name + "\n")
                elif dtype != "R":
                    ff.write('        CALL ps_diff("' + name +
                             '",ss1%' + name + ',ss2%' + name +
                             ',icheck,iout,ierr)\n')
                    ff.write('        CALL ps_conformal("' + name +
                             '",ss1%' + name + ',ss3%' + name +
                             ',iout,ierr)\n')
                    ff.write("        if(ierr.eq.0) ss3%" + name + " = ss1%" + name + "\n")
                elif section != "STATE_DATA" and section != "STATE_PROFILES":
                    ff.write('        CALL ps_diff("' + name +
                             '",ss1%' + name + ',ss2%' + name +
                             ',icheck,iout,ierr)\n')
                    ff.write('        CALL ps_conformal("' + name +
                             '",ss1%' + name + ',ss3%' + name +
                             ',iout,ierr)\n')
                    ff.write("        if(ierr.eq.0) ss3%" + name + " = ss1%" + name + "\n")
                else:
                    ff.write('        CALL ps_conformal("' + name +
                             '",ss2%' + name + ',ss3%' + name +
                             ',iout,ierr)\n')
                    ff.write('        CALL ps_conformal("' + name +
                             '",ss1%' + name + ',ss3%' + name +
                             ',iout,ierr)\n')
                    ff.write("        if(ierr.eq.0) ss3%" + name + " = w1*ss1%" + name +
                             " + (1.0_rspec-w1)*ss2%" + name + "\n")

                ff.write("     ELSE IF(ALLOCATED(ss1%" + name + ")) THEN\n")
                ff.write('        CALL ps_conformal("' + name +
                         '",ss1%' + name + ',ss3%' + name +
                         ',iout,ierr)\n')
                ff.write("        ss3%" + name + " = ss1%" + name + "\n")
                ff.write("     ELSE IF(ALLOCATED(ss2%" + name + ")) THEN\n")
                ff.write('        CALL ps_conformal("' + name +
                         '",ss2%' + name + ',ss3%' + name +
                         ',iout,ierr)\n')
                ff.write("        ss3%" + name + " = ss2%" + name + "\n")
                ff.write("     ENDIF\n")
                ff.write("  ENDIF\n")

            elif 'dimtype' not in descr:
                #  scalar, not an array dimension

                ff.write("\n")
                ff.write("  IF(cclist(ps_cnum_" + cur_component +
                         ").EQ.1) THEN\n")
                if section != "STATE_DATA" and section != "STATE_PROFILES":
                    ff.write('\n')
                    ff.write('     CALL ps_diff("' + name + '",ss1%' + name +
                             ',ss2%' + name + ',icheck,iout,ierr)\n')
                    ff.write("     ss3%" + name + " = ss1%" + name + "\n")
                elif dtype != "R":
                    ff.write('\n')
                    ff.write('     CALL ps_diff("' + name + '",ss1%' + name +
                             ',ss2%' + name + ',icheck,iout,ierr)\n')
                    ff.write("     ss3%" + name + " = ss1%" + name + "\n")
                else:
                    ff.write("     ss3%" + name + " = w1*ss1%" + name +
                             " + (1.0_rspec-w1)*ss2%" + name + "\n")

                ff.write("  ENDIF\n")

        outstr = "\nEND SUBROUTINE plasma_state_merge_" + repr(isub_count) + "\n"
        ff.write(outstr)

        isub_count = isub_count + 1
        #  end of sublist loop -- plasma_state_merge

    #---------------------------
    # plasma_state_copy...

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_copy_" + repr(isub_count)
        ff = f_xcopa[isub_count]

        arglist = "(cclist,ss,ss2,icopy_lists,iwant_1deq,partial_copy,ierr)\n"
        outcall = "  call " + sname + arglist
        f_xcopy.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)
        ff.write("""

  ! Copy scalars, and, arrays that are allocated
  !   ss2 <-- ss

  USE plasma_state_definition_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: cclist(ps_ccount)
                                ! list of components to merge
                                ! use ps_cc_all to merge all components

  TYPE (plasma_state) :: ss     ! state data to copy *from*
  TYPE (plasma_state) :: ss2    ! state data to copy *to*

  INTEGER, INTENT(IN) :: icopy_lists ! =1 to require copying of all lists

  INTEGER, INTENT(IN) :: iwant_1deq  ! =1 to request fcns of (rho_eq)
                                     !  even if cclist(ps_cnum_EQ).eq.0

  LOGICAL, INTENT(IN) :: partial_copy ! .TRUE. if full state copy not in effect

  INTEGER, INTENT(INOUT) :: ierr  ! completion flag (not cleared, set by error)

  !-------------------
  INTEGER :: iout
  !-------------------

  iout = SWIM_err_out_file

""")

        isub_count = isub_count + 1

    # end of sub-subroutine call & declaration loop for plasma_state_copy
    # now enter loop to write copy code for each state element

    isub_count = 0

    while ( isub_count < nsplit_sce ):
        ## print(" **** ",isub_count, isublists[isub_count])
        ff = f_xcopa[isub_count]

        ff.write('\n')
        ff.write("  !-----------------------------------------------\n")
        ff.write("  ! copy scalars and arrays that are allocated...\n")

        for key in isublists[isub_count]:
            descr = all_items[key]
            name = descr['name']
            dtype = descr['dtype']
            section = descr['section']
            cur_component = descr['component']

            if 'dlist' in descr:
                dlist = descr['dlist']
                #  array
                dimnamx="no"
                if len(dlist) == 1:
                    #  look for 1d standalone profiles vs. [rho_eq]
                    if cur_component == "EQ":
                        dimp0=dlist[0]
                        dimnamx=dimp0[1]

                if key in grid_items or 'rank_coord' in descr:
                    #  grid or profile
                    if 'rank_coord' in descr and key not in deriv_list:
                        if len(dlist) == descr['rank_coord']:
                            clear_idpart(ff,"  ",name,"single")
                        else:
                            clear_idpart(ff,"  ",name,"alloc")

                    copy_iftest(ff,cur_component,dimnamx,name)
                    if 'rank_coord' in descr and key not in deriv_list:
                        # profile (not a derivative): copy ID
                        if len(dlist) == descr['rank_coord']:
                            copy_idpart(ff,"     ",name,"single")
                        else:
                            copy_idpart(ff,"     ",name,"alloc")

                    ff.write("  ENDIF\n")

                elif key not in species_list_attrib:
                    #  non-grid, non-profile, non-species list array
                    #  copy of arrays are allocated and switches set
                    ff.write("  IF( ((ALLOCATED(ss2%" + name +
                             ")).AND.(ALLOCATED(ss%" + name + "))).AND. &\n")
                    ff.write("       ((icopy_lists.eq.1).OR.(cclist(ps_cnum_"+
                             cur_component + ").EQ.1)) ) THEN\n")
                    ff.write("     ss2%" + name + " = ss%" + name + "\n")
                    ff.write("  ENDIF\n")

                else:
                    #  species list item -- if arrays are allocated, copy
                    ff.write("  IF((ALLOCATED(ss2%" + name +
                             ")).AND.(ALLOCATED(ss%" + name + "))) THEN\n")
                    ff.write("     ss2%" + name + " = ss%" + name + "\n")
                    ff.write("  ENDIF\n")


            elif 'dimtype' not in descr:
                #  scalar, not an array dimension
                if key in species_list_attrib:
                    ff.write("  ss2%" + name + " = ss%" + name + "\n")
                else:
                    ff.write("  IF((cclist(ps_cnum_" + cur_component +
                             ").EQ.1).OR.(icopy_lists.EQ.1)) THEN\n")
                    ff.write("     ss2%" + name + " = ss%" + name + "\n")
                    ff.write("  ENDIF\n")

        sname = "plasma_state_copy_" + repr(isub_count)
        ff.write("\nEND SUBROUTINE " + sname + "\n")

        isub_count = isub_count + 1

    # sub-subroutines written; now write plasma_state_copy trailer

    f_xcopy.write("""
  IF(partial_copy) THEN
     IF(ss2%lock_sim_init.EQ.ps_locked) THEN
        WRITE(iout,*) ' %plasma_state_copy: partial copy: Simulation_Init unlocked.'
        ss2%lock_sim_init = ps_unlocked
     ENDIF
  ENDIF

  ! set hash codes of new copy of state

  ilock_mdescr = ss2%lock_machine_descr
  ilock_sconfig = ss2%lock_shot_config
  ilock_siminit = ss2%lock_sim_init

  ss2%lock_machine_descr = ps_unlocked
  ss2%lock_shot_config = ps_unlocked
  ss2%lock_sim_init = ps_unlocked

  CALL plasma_state_hash_code(ss2,ps_cc_all,iertmp)
  IF(idata_only.NE.1) THEN
     ss2%hash_codes = ss2%new_hash_codes
  ENDIF

  if(cclist(ps_cnum_EQ).eq.1) THEN
     ! copy aux hash codes
""")

    astr = 'ps_hash_aux1:ps_hash_aux' + repr(num_aux_hash_pairs) + '+1'
    f_xcopy.write('     ss2%hash_codes(' + astr + ') = ss%hash_codes(' + astr + ')\n')
    f_xcopy.write('     ss2%new_hash_codes(' + astr + ') = ss%new_hash_codes(' + astr + ')\n')

    f_xcopy.write("""
  endif

  ss2%lock_machine_descr = ilock_mdescr
  ss2%lock_shot_config = ilock_sconfig
  ss2%lock_sim_init = ilock_siminit

contains

  subroutine copydim(name,fromdim,todim)

    character*(*), intent(in) :: name
    integer, intent(in) :: fromdim
    integer, intent(inout) :: todim

    if (fromdim.eq.0) then
       continue             ! (old value retained in target state)

    else if(todim.eq.0) then
       if(idata_only.eq.0) then
          todim = fromdim      ! (dimension value is copied)
       endif

    else if(todim.ne.fromdim) then
       ierr = 1             ! (both non-zero and values are unequal)
       write(iout,*) ' ?PLASMA_STATE_COPY: target state array dimension has'
       write(iout,*) '  inconsistent value: dimension name is: '//trim(name)
       write(iout,*) '  source state value: ',fromdim,'; target state value: ',todim
    endif
  end subroutine copydim

END SUBROUTINE plasma_state_copy
""")

def copy_idpart(f,indent,name,type):

    if type == "single":
        f.write(indent + "IF(.not.partial_copy) THEN\n")
        f.write(indent + "   ss2%id_" + name + " = ss%id_" + name + "\n")
        f.write(indent + "ENDIF\n")

    else:
        f.write(indent + "IF(ALLOCATED(ss2%id_" + name + ")) THEN\n")
        f.write(indent + "   IF(.not.partial_copy) THEN\n")
        f.write(indent + "      ss2%id_" + name + " = ss%id_" + name + "\n")
        f.write(indent + "   ENDIF\n")
        f.write(indent + "ENDIF\n")

def clear_idpart(f,indent,name,type):

    if type == "single":
        f.write(indent + "IF(partial_copy) THEN\n")
        f.write(indent + "   ss2%id_" + name + " = 0\n")
        f.write(indent + "ENDIF\n")

    else:
        f.write(indent + "IF(ALLOCATED(ss2%id_" + name + ")) THEN\n")
        f.write(indent + "   IF(partial_copy) THEN\n")
        f.write(indent + "      ss2%id_" + name + " = 0\n")
        f.write(indent + "   ENDIF\n")
        f.write(indent + "ENDIF\n")

def copy_iftest(f,cc,eq1d,name):

    # start IF branch for copy; copy array
    # trailing ENDIF is NOT written

    if eq1d != "nrho_eq":
        trlr = ')) THEN'
    else:
        trlr = '.OR.(iwant_1deq.eq.1))) THEN'

    f.write('  IF((ALLOCATED(ss%' + name
            + ').AND.ALLOCATED(ss2%' + name + ')).AND. &\n')
    f.write('        ((cclist(ps_cnum_' + cc + ').EQ.1)' + trlr + '\n')
    f.write("     ss2%" + name + " = ss%" + name + "\n")

def plasma_state_write_def():

    # write the plasma_state_definition_mod module
    # this contains the definition of the plasma_state and ps_list_window
    # derived types + some methods for the ps_list_window derived type.
    # The module contains no data.

    print("...plasma_state_write_def()...")

    iilist = list(all_items.keys())
    iilist.sort()

    cclist = list(all_components.keys())
    cclist.sort()

    sslist = {}
    for section in all_sections:
        sslist[section] = {}
        for component in cclist:
            sslist[section][component] = []

    for key in iilist:
        descr = all_items[key]
        ss = descr['section']
        cc = descr['component']

        sslist[ss][cc].append(key)

    for section in all_sections:
        f_sdef.write('\n')
        f_sdef.write('     !===============================================\n')
        f_sdef.write('     ! Section: ' + section + '\n')

        for component in cclist:
            ilist = sslist[section][component]
            if len(iilist) > 0:
                f_sdef.write('\n')
                f_sdef.write('     !----------------------------\n')
                f_sdef.write('     ! Component: ' + component + '\n')

            # dimensions first (if any...)
            for key in ilist:
                descr = all_items[key]
                if 'dimtype' in descr:
                    dimtype = descr['dimtype']
                    f_sdef.write('\n')
                    if dimtype in all_coords:
                        f_sdef.write('     ! ** ' + dimtype + ' Grid Dimension **\n')
                    elif dimtype == "S":
                        f_sdef.write('     ! ** Species List Dimension **\n')

                    elif dimtype == "L":
                        f_sdef.write('     ! ** Item List Dimension **\n')

                    else:
                        f_sdef.write('     ! ** (Ad hoc) Array Dimension **\n')

                    wr_decl(f_sdef,"     ",descr)

            # now everything else...
            for key in ilist:
                descr = all_items[key]
                if 'dimtype' not in descr:
                    wr_decl(f_sdef,"     ",descr)

    f_sdef.write("""

  END TYPE plasma_state

  !-------------------------------------------------------------------------
  !   ps_list_window TYPE definition
  !      this is an aid to state visualization software
  !-------------------------------------------------------------------------

  TYPE :: ps_list_window

    CHARACTER*32 :: list_name = ' '   ! name of list
    INTEGER :: size = 0               ! size of list (# of elements)

    CHARACTER*32, DIMENSION(:), POINTER :: list_elems => NULL()
                                           ! sorted list element names

    CHARACTER*32, DIMENSION(:), POINTER :: elem_units => NULL()
                                           ! element units labels

    CHARACTER*256, DIMENSION(:), POINTER :: elem_descr => NULL()
                                           ! element descriptions

    CHARACTER*32, DIMENSION(:), POINTER :: component => NULL()
                                           ! component membership of element

    CHARACTER*32, DIMENSION(:), POINTER :: val_field => NULL()
                                           ! element value as character string

    CHARACTER*256, DIMENSION(:), POINTER :: c256val => NULL()
                                           ! CHAR*256 value (e.g. file path)

    INTEGER, DIMENSION(:), POINTER :: ival => NULL()
                                           ! INTEGER value

    REAL(KIND=rspec), DIMENSION(:), POINTER :: fval => NULL()
                                           ! REAL(KIND=rspec) value

    INTEGER, DIMENSION(:), POINTER :: vecsizes => NULL()
                                           ! vector sizes

    INTEGER, DIMENSION(:,:), POINTER :: ivec => NULL()
                                           ! INTEGER vectors

    REAL(KIND=rspec), DIMENSION(:,:), POINTER :: fvec => NULL()
                                           ! REAL(KIND=rspec) vectors

    !  species list data...

    INTEGER, DIMENSION(:), POINTER :: spectype => NULL()
                                           ! type of species e.g. ps_therm_ion

    INTEGER, DIMENSION(:), POINTER :: qval => NULL()
                                           ! ion charge

    INTEGER, DIMENSION(:), POINTER :: qatom => NULL()
                                           ! atomic number

    REAL(KIND=rspec), DIMENSION(:), POINTER :: awght => NULL()
                                           ! atomic weight
  END TYPE ps_list_window

  CONTAINS

    SUBROUTINE load_ps_container(iarray,ptr)
      INTEGER :: iarray(*)
      TYPE (plasma_state_container) :: ptr

      ptr = transfer(iarray(1:ps_container_size),ptr)
    END SUBROUTINE load_ps_container

    SUBROUTINE unload_ps_container(ptr,iarray)
      TYPE (plasma_state_container) :: ptr
      INTEGER :: iarray(*)

      iarray(1:ps_container_size) = transfer(ptr,iarray(1:ps_container_size))
    END SUBROUTINE unload_ps_container

    SUBROUTINE free_window(w)

      TYPE (ps_list_window) :: w

      w%list_name = ' '
      w%size = 0

      if(associated(w%list_elems)) deallocate(w%list_elems)
      if(associated(w%elem_descr)) deallocate(w%elem_descr)
      if(associated(w%component)) deallocate(w%component)
      if(associated(w%elem_units)) deallocate(w%elem_units)
      if(associated(w%val_field)) deallocate(w%val_field)
      if(associated(w%c256val)) deallocate(w%c256val)
      if(associated(w%ival)) deallocate(w%ival)
      if(associated(w%fval)) deallocate(w%fval)
      if(associated(w%vecsizes)) deallocate(w%vecsizes)
      if(associated(w%ivec)) deallocate(w%ivec)
      if(associated(w%fvec)) deallocate(w%fvec)
      if(associated(w%spectype)) deallocate(w%spectype)
      if(associated(w%qval)) deallocate(w%qval)
      if(associated(w%qatom)) deallocate(w%qatom)
      if(associated(w%awght)) deallocate(w%awght)

    END SUBROUTINE free_window

    SUBROUTINE clone_window(w1,w2)

      TYPE (ps_list_window) :: w1,w2

      call free_window(w2)

      w2%list_name = w1%list_name
      w2%size = w1%size

      if(associated(w1%list_elems)) then
         allocate(w2%list_elems(size(w1%list_elems)))
         w2%list_elems = w1%list_elems
      endif

      if(associated(w1%elem_descr))  then
         allocate(w2%elem_descr(size(w1%elem_descr)))
         w2%elem_descr = w1%elem_descr
      endif

      if(associated(w1%component))  then
         allocate(w2%component(size(w1%component)))
         w2%component = w1%component
      endif

      if(associated(w1%elem_units))  then
         allocate(w2%elem_units(size(w1%elem_units)))
         w2%elem_units = w1%elem_units
      endif

      if(associated(w1%val_field))  then
         allocate(w2%val_field(size(w1%val_field)))
         w2%val_field = w1%val_field
      endif

      if(associated(w1%c256val))  then
         allocate(w2%c256val(size(w1%c256val)))
         w2%c256val = w1%c256val
      endif

      if(associated(w1%ival))  then
         allocate(w2%ival(size(w1%ival)))
         w2%ival = w1%ival
      endif

      if(associated(w1%fval))  then
         allocate(w2%fval(size(w1%fval)))
         w2%fval = w1%fval
      endif

      if(associated(w1%vecsizes))  then
         allocate(w2%vecsizes(size(w1%vecsizes)))
         w2%vecsizes = w1%vecsizes
      endif

      if(associated(w1%ivec))  then
         allocate(w2%ivec(size(w1%ivec,1),size(w1%ivec,2)))
         w2%ivec = w1%ivec
      endif

      if(associated(w1%fvec))  then
         allocate(w2%fvec(size(w1%fvec,1),size(w1%fvec,2)))
         w2%fvec = w1%fvec
      endif

      if(associated(w1%spectype))  then
         allocate(w2%spectype(size(w1%spectype)))
         w2%spectype = w1%spectype
      endif

      if(associated(w1%qval))  then
         allocate(w2%qval(size(w1%qval)))
         w2%qval = w1%qval
      endif

      if(associated(w1%qatom))  then
         allocate(w2%qatom(size(w1%qatom)))
         w2%qatom = w1%qatom
      endif

      if(associated(w1%awght))  then
         allocate(w2%awght(size(w1%awght)))
         w2%awght = w1%awght
      endif

    END SUBROUTINE clone_window

  !-------------------------------------------------------------------------
  !
  !   Data declarations -- NONE
  !
  !-------------------------------------------------------------------------

END MODULE plasma_state_definition_mod
""")

def wr_decl(f,indent,descr):
    # write a single element declaration

    ftype = descr['ftype']
    name = descr['name']

    f.write('\n')

    if 'dlist' not in descr:
        # scalar

        init = descr['init']
        str = indent + ftype + ' :: ' + name + ' = ' + init + '\n'
        f.write(str)

    else:
        dlist = descr['dlist']
        cstr = colon_str(dlist)
        str = indent + ftype + ', DIMENSION' + cstr
        str = str + ', ALLOCATABLE :: ' + name + '\n'

        if 'methkey' in descr:
            methkey = descr['methkey']

        if 'methkey' in descr and methkey != "Hermite_derivative":
            # profile array
            rank_coord = descr['rank_coord']
            if rank_coord == len(dlist):
                # standalone profile; integer ID
                str_id = indent + 'INTEGER :: id_' + name + ' = 0\n'
            else:
                # profile array; integer ID array
                cstr = colon_str(dlist[rank_coord:])
                str_id = indent + 'INTEGER, DIMENSION' + cstr
                str_id = str_id + ', ALLOCATABLE :: id_' + name + '\n'

            f.write(str_id)

        if 'methkey' in descr and methkey == "Hermite_derivative":
            f.write(indent + "  ! (Hermite derivative):\n")

        f.write(str)

    for cmt in descr['cmts']:
        f.write(indent + "  !" + cmt + "\n")

    line = descr['spec']
    f.write(indent + "  !" + line + "\n")

def colon_str(dlist):

    # generate string of form "(:)" or "(:,:)" or ... depending on length
    # of dlist

    ilen = len(dlist)
    icount = 0

    cstr = "("

    for dimpair in dlist:
        icount = icount + 1
        if icount < ilen:
            cstr = cstr + ":,"
        else:
            cstr = cstr + ":)"

    return cstr

def plasma_state_write_calloc():

    # write code that allocates "window" objects for cstate
    # NOTE: the generated logic here should match that of
    # plasma_state_write_contents()... this procedure generates
    # the code that allocates the structures; "contents" generates
    # the code that fills the structures...

    print("...plasma_state_write_calloc()...")

    # header written on f_pscon above...

    elist = list(all_enum_dims.keys())
    elist.sort()

    f_pscon.write("  ! Enumeration Dimensions\n")

    for key in elist:
        descr = all_items[key]
        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component
        wr_contents_clause(f_pscon,psc,'ps_enum_dims',name,"BOTH")

    f_pscon.write("  ! Species Lists\n")

    for key in species_list_items:
        descr = all_items[key]
        if 'dimtype' in descr:
            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            wr_contents_clause(f_pscon,psc,'ps_species_lists',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! Item Lists\n")

    for key in item_list_items:
        descr = all_items[key]
        if 'dimtype' in descr:
            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            wr_contents_clause(f_pscon,psc,'ps_item_lists',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! Grids\n")

    for key in grid_items:
        descr = all_items[key]
        if 'dimtype' in descr:
            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            wr_contents_clause(f_pscon,psc,'ps_grid_dims',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! Machine Description\n")

    for key in mdescr:
        descr = all_items[key]
        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        wr_contents_clause(f_pscon,psc,'ps_machine_descr',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! Shot Configuration\n")

    for key in sconfig:
        descr = all_items[key]
        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        wr_contents_clause(f_pscon,psc,'ps_shot_config',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! Simulation Init\n")

    for key in simcon:
        descr = all_items[key]
        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        wr_contents_clause(f_pscon,psc,'ps_sim_control',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! State Data\n")

    for key in state_data_list:
        descr = all_items[key]
        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        wr_contents_clause(f_pscon,psc,'ps_data',name,"BOTH")

    f_pscon.write("\n")
    f_pscon.write("  ! Profiles\n")

    for key in profile_list:
        descr = all_items[key]
        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        wr_contents_clause(f_pscon,psc,'ps_profiles',name,"BOTH")

    #  now, all groups...

    glist = list(all_groups.keys())
    glist.sort()

    for grpname in glist:
        mlist = all_groups[grpname]

        component = mlist[0]
        psc = "ps_cnum_" + component

        g1 = mlist[1]  # 1st member of group
        descr = all_items[g1]
        section = descr['section']

        cat = psw_catfind(section)

        wr_contents_clause(f_pscon,psc,cat,grpname,"BOTH")

    #  write the trailer...

    f_pscon.write("""

  w%size = isize

  allocate(w%list_elems(isize)); w%list_elems = ' '
  allocate(w%elem_descr(isize)); w%elem_descr = ' '
  allocate(w%ival(isize)); w%ival = 0
  allocate(w%component(isize)); w%component = ' '

  allocate(w%vecsizes(isize)); w%vecsizes = 0

  if((itype.eq.ps_data).or.(itype.eq.ps_profiles).or. &
       (itype.eq.ps_machine_descr).or.(itype.eq.ps_shot_config).or. &
       (itype.eq.ps_sim_control)) then

     allocate(w%c256val(isize)); w%c256val = ' '
     allocate(w%elem_units(isize)); w%elem_units = ' '
     if(itype.eq.ps_profiles) then
        allocate(w%val_field(isize)); w%val_field = ' '
     endif

  else

     allocate(w%val_field(isize)); w%val_field = ' '

  endif

END SUBROUTINE ps_contents_alloc
""")

def plasma_state_write_contents():

    # write code that allocates "window" objects for cstate
    # NOTE: the generated logic here should match that of
    # plasma_state_write_calloc()... that procedure generates
    # the code that allocates the structures; this one generates
    # the code that fills the structures...

    print("...plasma_state_write_contents()...")

    # header written on f_pscon2 above...

    elist = list(all_enum_dims.keys())
    elist.sort()

    f_pscon2.write("  ! Enumeration Dimensions\n")

    for ekey in elist:
        descr = all_items[ekey]
        name = descr['name']
        if 'dlist' in descr:
            key = 'MAX_' + ekey
            gen_dim1 = name + gen_dimstr("",descr['dlist'])
        else:
            key = ekey
            gen_dim1 = name

        descr = all_items[key]

        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        cmts = descr['cmts']
        cmt = cmts[0]
        dcmt = cmt_dcmt(key,cmt)

        units = 'integer'
        evar = key.lower()

        wr_contents_clause(f_pscon2,psc,'ps_enum_dims',name,"START")

        f_pscon2.write('        w%list_elems(isize) = "' + name + '"\n')
        f_pscon2.write('        w%elem_descr(isize) = ' + dcmt + '\n')
        f_pscon2.write('        w%component(isize) = "' + component + '"\n')
        f_pscon2.write('        w%ival(isize) = ss%' + evar + '\n')
        f_pscon2.write('        w%val_field(isize) = "' + gen_dim1 + '"\n')

        wr_contents_clause(f_pscon2,psc,'ps_machine_descr',name,"FINISH")

    f_pscon2.write("  ! Species Lists\n")

    for key in species_list_items:
        descr = all_items[key]
        if 'dimtype' in descr:
            # look up variable containing names associated with this dimension
            sname = descr['rootname'].upper()

            name_var = descr['name_var']
            nkey = name_var.upper()
            descr = all_items[nkey]

            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            cmts = descr['cmts']
            cmt = cmts[0]
            dcmt = cmt_dcmt(key,cmt)

            dlist = descr['dlist']

            gen_dim1 = gen_dimstr("",dlist)

            wr_contents_clause(f_pscon2,psc,'ps_species_lists',sname,"START")

            f_pscon2.write('        w%list_elems(isize) = "' + sname + '"\n')
            f_pscon2.write('        w%elem_descr(isize) = ' + dcmt + '\n')
            f_pscon2.write('        w%component(isize) = "' + component + '"\n')
            f_pscon2.write('        w%val_field(isize) = "' + gen_dim1 + '"\n')
            psw_ival_expr(f_pscon2,dlist[0])

            wr_contents_clause(f_pscon2,psc,'ps_species_lists',sname,"FINISH")

    f_pscon2.write("\n")
    f_pscon2.write("  ! Item Lists\n")

    for key in item_list_items:
        descr = all_items[key]
        if 'dimtype' in descr:
            # look up variable containing names associated with this dimension
            name_var = descr['name_var']
            nkey = name_var.upper()
            descr = all_items[nkey]

            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            cmts = descr['cmts']
            cmt = cmts[0]
            dcmt = cmt_dcmt(key,cmt)

            dlist = descr['dlist']

            gen_dim1 = gen_dimstr("",dlist)

            wr_contents_clause(f_pscon2,psc,'ps_item_lists',name,"START")

            f_pscon2.write('        w%list_elems(isize) = "' + name + '"\n')
            f_pscon2.write('        w%elem_descr(isize) = ' + dcmt + '\n')
            f_pscon2.write('        w%component(isize) = "' + component + '"\n')
            f_pscon2.write('        w%val_field(isize) = "' + gen_dim1 + '"\n')
            psw_ival_expr(f_pscon2,dlist[0])

            wr_contents_clause(f_pscon2,psc,'ps_item_lists',name,"FINISH")

    f_pscon2.write("\n")
    f_pscon2.write("  ! Grids\n")

    for key in grid_items:
        descr = all_items[key]
        if 'dimtype' in descr:
            gridname = descr['gridname']
            nkey = gridname.upper()
            descr = all_items[nkey]

            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            cmts = descr['cmts']
            cmt = cmts[0]
            dcmt = cmt_dcmt(key,cmt)

            dlist = descr['dlist']

            gen_dim1 = gen_dimstr("",dlist)

            wr_contents_clause(f_pscon2,psc,'ps_grid_dims',name,"START")

            f_pscon2.write('        w%list_elems(isize) = "' + name + '"\n')
            f_pscon2.write('        w%elem_descr(isize) = ' + dcmt + '\n')
            f_pscon2.write('        w%component(isize) = "' + component + '"\n')
            f_pscon2.write('        w%val_field(isize) = "' + gen_dim1 + '"\n')
            psw_ival_expr(f_pscon2,dlist[0])

            wr_contents_clause(f_pscon2,psc,'ps_grid_dims',name,"FINISH")

    f_pscon2.write("\n")
    f_pscon2.write("  ! Machine Description\n")
    f_pscon2.write("\n")
    f_pscon2.write("  CALL plasma_state_contents_mdescr(itype,icomponent,ss,w,isize)\n")
    f_pscon2.write("\n")

    gg = open("plasma_state_contents_mdescr.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_mdescr(itype,icomponent,ss,w,isize)


  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(INOUT) :: isize  ! incremented size...

  !local:

  INTEGER :: id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------
"""%version_id)

    for key in mdescr:
        descr = all_items[key]

        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        cmts = descr['cmts']
        cmt = cmts[0]
        dcmt = cmt_dcmt(key,cmt)

        dtype = descr['dtype']
        if dtype == "I":
            units = 'integer'
        elif dtype == "N" or dtype == "F" or dtype[0] == "C":
            units = 'character'
        else:
            units = descr['units']

        if 'dlist' in descr:
            dlist = descr['dlist']
            dimval = gen_dimstr("",dlist)
        else:
            dimval = "scalar"

        wr_contents_clause(gg,psc,'ps_machine_descr',name,"START")

        gg.write('        w%list_elems(isize) = "' + name + '"\n')
        gg.write('        w%elem_descr(isize) = ' + dcmt + '\n')
        gg.write('        w%component(isize) = "' + component + '"\n')
        gg.write('        w%elem_units(isize) = "' + units + '"\n')
        pscon2_valdef(gg,name,dimval,descr)

        wr_contents_clause(gg,psc,'ps_machine_descr',name,"FINISH")

    gg.write('END SUBROUTINE plasma_state_contents_mdescr\n')
    gg.close()

    f_pscon2.write("\n")
    f_pscon2.write("  ! Shot Configuration\n")
    f_pscon2.write("\n")
    f_pscon2.write("  CALL plasma_state_contents_sconfig(itype,icomponent,ss,w,isize)\n")
    f_pscon2.write("\n")

    gg = open("plasma_state_contents_sconfig.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_sconfig(itype,icomponent,ss,w,isize)


  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(INOUT) :: isize  ! incremented size...

  !local:

  INTEGER :: id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------
"""%version_id)

    for key in sconfig:
        descr = all_items[key]

        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        cmts = descr['cmts']
        cmt = cmts[0]
        dcmt = cmt_dcmt(key,cmt)

        dtype = descr['dtype']
        if dtype == "I":
            units = 'integer'
        elif dtype == "N" or dtype == "F" or dtype[0] == "C":
            units = 'character'
        else:
            units = descr['units']

        if 'dlist' in descr:
            dlist = descr['dlist']
            dimval = gen_dimstr("",dlist)
        else:
            dimval = "scalar"

        wr_contents_clause(gg,psc,'ps_shot_config',name,"START")

        gg.write('        w%list_elems(isize) = "' + name + '"\n')
        gg.write('        w%elem_descr(isize) = ' + dcmt + '\n')
        gg.write('        w%component(isize) = "' + component + '"\n')
        gg.write('        w%elem_units(isize) = "' + units + '"\n')
        pscon2_valdef(gg,name,dimval,descr)

        wr_contents_clause(gg,psc,'ps_shot_config',name,"FINISH")

    gg.write('END SUBROUTINE plasma_state_contents_sconfig\n')
    gg.close()

    f_pscon2.write("\n")
    f_pscon2.write("  ! Simulation Init\n")
    f_pscon2.write("\n")
    f_pscon2.write("  CALL plasma_state_contents_sini(itype,icomponent,ss,w,isize)\n")
    f_pscon2.write("\n")

    gg = open("plasma_state_contents_sini.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_sini(itype,icomponent,ss,w,isize)


  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(INOUT) :: isize  ! incremented size...

  !local:

  INTEGER :: id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------
"""%version_id)

    for key in simcon:
        descr = all_items[key]

        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        cmts = descr['cmts']
        cmt = cmts[0]
        dcmt = cmt_dcmt(key,cmt)

        dtype = descr['dtype']
        if dtype == "I":
            units = 'integer'
        elif dtype == "N" or dtype == "F" or dtype[0] == "C":
            units = 'character'
        else:
            units = descr['units']

        if 'dlist' in descr:
            dlist = descr['dlist']
            dimval = gen_dimstr("",dlist)
        else:
            dimval = "scalar"

        wr_contents_clause(gg,psc,'ps_sim_control',name,"START")

        gg.write('        w%list_elems(isize) = "' + name + '"\n')
        gg.write('        w%elem_descr(isize) = ' + dcmt + '\n')
        gg.write('        w%component(isize) = "' + component + '"\n')
        gg.write('        w%elem_units(isize) = "' + units + '"\n')
        pscon2_valdef(gg,name,dimval,descr)

        wr_contents_clause(gg,psc,'ps_sim_control',name,"FINISH")

    gg.write('END SUBROUTINE plasma_state_contents_sini\n')
    gg.close()

    f_pscon2.write("\n")
    f_pscon2.write("  ! State Data\n")
    f_pscon2.write("\n")
    f_pscon2.write("  CALL plasma_state_contents_sdata(itype,icomponent,ss,w,isize)\n")
    f_pscon2.write("\n")

    gg = open("plasma_state_contents_sdata.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_contents_sdata(itype,icomponent,ss,w,isize)


  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(INOUT) :: isize  ! incremented size...

  !local:

  INTEGER :: id,inum
  INTEGER :: i1,i2,i3,i4

  !----------------
"""%version_id)

    for key in state_data_list:
        descr = all_items[key]

        name = descr['name']
        component = descr['component']
        psc = "ps_cnum_" + component

        cmts = descr['cmts']
        cmt = cmts[0]
        dcmt = cmt_dcmt(key,cmt)

        dtype = descr['dtype']
        if dtype == "I":
            units = 'integer'
        elif dtype == "N" or dtype == "F" or dtype[0] == "C":
            units = 'character'
        else:
            units = descr['units']

        if 'dlist' in descr:
            dlist = descr['dlist']
            dimval = gen_dimstr("",dlist)
        else:
            dimval = "scalar"

        wr_contents_clause(gg,psc,'ps_data',name,"START")

        gg.write('        w%list_elems(isize) = "' + name + '"\n')
        gg.write('        w%elem_descr(isize) = ' + dcmt + '\n')
        gg.write('        w%component(isize) = "' + component + '"\n')
        gg.write('        w%elem_units(isize) = "' + units + '"\n')
        pscon2_valdef(gg,name,dimval,descr)

        wr_contents_clause(gg,psc,'ps_data',name,"FINISH")

    gg.write('END SUBROUTINE plasma_state_contents_sdata\n')
    gg.close()

    f_pscon2.write("\n")
    f_pscon2.write("  ! Profiles (split large file for faster compiling)\n")
    f_pscon2.write("\n")
    for i in range(0, nsplit_sce):
        s = "  CALL plasma_state_contents_profs_" + str(i) + "(itype,icomponent,ss,w,isize)\n"
        f_pscon2.write(s)
    f_pscon2.write("\n")

    isublists = sublist_gen(profile_list)

    for isub_count in range(0, nsplit_sce):
        ff = f_pscon3[isub_count]

        ff.write("""
SUBROUTINE plasma_state_contents_profs_%s(itype,icomponent,ss,w,isize)


  ! fill window structure with requested table of contents

  USE plasma_state_mod
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: itype   ! type of table of contents
                                 ! values such as ps_grid_dims or ps_profiles

  INTEGER, INTENT(IN) :: icomponent(ps_ccount) ! component flags
             ! icomponent(j).eq.1 => component j active
             ! e.g. icomponent(ps_cnum_EQ).eq.1 => EQ component is active

  TYPE (plasma_state) :: ss      ! state examined
  TYPE (ps_list_window) :: w     ! list window structure filled

  INTEGER, INTENT(INOUT) :: isize  ! incremented size...

  !----------------
"""%str(isub_count))

        for key in isublists[isub_count]:
            descr = all_items[key]

            name = descr['name']
            component = descr['component']
            psc = "ps_cnum_" + component

            cmts = descr['cmts']
            cmt = cmts[0]
            dcmt = cmt_dcmt(key,cmt)

            units = descr['units']

            dlist = descr['dlist']
            dimval = gen_dimstr("",dlist)

            methkey = descr['methkey']
            factor = descr['factor']
            if factor == " ":
                methprod = methkey
            elif factor in ["PS_DV","PS_DA"]:
                methprod = methkey + "*" + factor[3:]
            else:
                methprod = methkey + "*" + factor

            wr_contents_clause(ff,psc,'ps_profiles',name,"START")

            ff.write('        w%list_elems(isize) = "' + name + '"\n')
            ff.write('        w%elem_descr(isize) = ' + dcmt + '\n')
            ff.write('        w%component(isize) = "' + component + '"\n')
            ff.write('        w%elem_units(isize) = "' + units + '"\n')
            ff.write('        w%c256val(isize) = "' + dimval + '"\n')
            ff.write('        IF(ALLOCATED(ss%' + name + ')) THEN\n')
            ff.write('           IF(ss%hash_codes(ps_phash_' + name + ').LT.0) THEN\n')
            ff.write('              w%ival(isize) = 0\n')
            ff.write('           ELSE\n')

            ff.write('              w%ival(isize) = 1\n')
            ff.write('           ENDIF\n')
            ff.write('        ELSE\n')
            ff.write('           w%ival(isize) = -1\n')
            ff.write('        ENDIF\n')
            ff.write('        w%val_field(isize) = "' + methprod + '"\n')

            wr_contents_clause(ff,psc,'ps_profiles',name,"FINISH")

        ff.write('END SUBROUTINE plasma_state_contents_profs_%s\n'%str(isub_count))

    f_pscon2.write('\n')
    f_pscon2.write("  ! Profile groups\n")

    #  now, all groups...

    glist = list(all_groups.keys())
    glist.sort()

    for grpname in glist:
        mlist = all_groups[grpname]

        component = mlist[0]
        psc = "ps_cnum_" + component

        grpstr = "{"
        ilen = len(mlist[1:])
        icount = 0
        for g in mlist[1:]:
            icount = icount + 1
            descr = all_items[g]
            name = descr['name']
            if icount < ilen:
                grpstr = grpstr + name + ","
            else:
                grpstr = grpstr + name + "}"

        section = descr['section']
        dtype = descr['dtype']
        if dtype == "I":
            units = 'integer'
        elif dtype == "N" or dtype == "F" or dtype[0] == "C":
            units = 'character'
        else:
            units = descr['units']

        methprod = "(none)"
        if 'dlist' in descr:
            dlist = descr['dlist']
            dimval = gen_dimstr("",dlist)
        else:
            dimval = "scalar"

        if 'methkey' in descr:
            methkey = descr['methkey']
            factor = descr['factor']
            if factor == " ":
                methprod = methkey
            elif factor in ["PS_DV","PS_DA"]:
                methprod = methkey + "*" + factor[3:]
            else:
                methprod = methkey + "*" + factor

        cat = psw_catfind(section)

        wr_contents_clause(f_pscon2,psc,cat,grpname,"START")
        f_pscon2.write('        w%list_elems(isize) = "' + grpname + '"\n')
        if len(grpstr) < 80:
            f_pscon2.write('        w%elem_descr(isize) = "' + grpstr + '"\n')
        else:
            sgrpa = grpstr.split(",")
            ilgrpa = len(sgrpa)
            icgrpa = 1
            f_pscon2.write('        w%elem_descr(isize) = "' + sgrpa[0] + ',"// &\n')
            for sgrp in sgrpa[1:]:
                icgrpa = icgrpa + 1
                if icgrpa < ilgrpa:
                    f_pscon2.write('             "' + sgrp + ',"// &\n')
                else:
                    f_pscon2.write('             "' + sgrp + '"\n')

        f_pscon2.write('        w%component(isize) = "' + component + '"\n')
        f_pscon2.write('        w%elem_units(isize) = "' + units + '"\n')
        if dimval == "scalar":
            f_pscon2.write('        w%c256val(isize) = "scalar"\n')
            f_pscon2.write('        w%ival(isize) = 2\n')
        else:
            f_pscon2.write('        w%c256val(isize) = "' + dimval + '"\n')
            f_pscon2.write('        IF(ALLOCATED(ss%' + name + ')) THEN\n')
            f_pscon2.write('           w%ival(isize) = 2\n')
            f_pscon2.write('        ELSE\n')
            f_pscon2.write('           w%ival(isize) = -2\n')
            f_pscon2.write('        ENDIF\n')
        if methprod != "(none)":
            f_pscon2.write('        w%val_field(isize) = "' + methprod + '"\n')

        wr_contents_clause(f_pscon2,psc,cat,grpname,"FINISH")


    #  write the trailer...

    f_pscon2.write("\n")
    f_pscon2.write("END SUBROUTINE plasma_state_contents\n")

def pscon2_valdef(ff,name,dimval,descr):

    # write ival = -1 for unallocated array
    #            =  0 for scalar or allocated array, all elements of
    #                 which are zero or blank
    #            =  1 for scalar or allocated array, some elements of
    #                 which are non-zero or non-blank

    dtype = descr['dtype']

    if dimval == "scalar":
        ff.write('        w%c256val(isize) = "scalar"\n')
        if dtype == "R":
            ff.write('        IF(ss%' + name + '.EQ.0.0_rspec) THEN\n')
            ff.write('           w%ival(isize) = 0\n')
            ff.write('        ELSE\n')
            ff.write('           w%ival(isize) = 1\n')
            ff.write('        ENDIF\n')
        elif dtype == "I":
            ff.write('        IF((ss%' + name + '.EQ.0).OR.(ss%' +
                           name + '.EQ.ps_unInit)) THEN\n')
            ff.write('           w%ival(isize) = 0\n')
            ff.write('        ELSE\n')
            ff.write('           w%ival(isize) = 1\n')
            ff.write('        ENDIF\n')
        else:
            ff.write('        IF(ss%' + name + '.EQ." ") THEN\n')
            ff.write('           w%ival(isize) = 0\n')
            ff.write('        ELSE\n')
            ff.write('           w%ival(isize) = 1\n')
            ff.write('        ENDIF\n')

    else:
        ff.write('        w%c256val(isize) = "' + dimval + '"\n')
        ff.write('        IF(ALLOCATED(ss%' + name + ')) THEN\n')
        if dtype == "R":
            ff.write('           IF(MAXVAL(ABS(ss%' + name + ')).EQ.0.0_rspec) THEN\n')
            ff.write('              w%ival(isize) = 0\n')
            ff.write('           ELSE\n')
            ff.write('              w%ival(isize) = 1\n')
            ff.write('           ENDIF\n')
        elif dtype == "I":
            ff.write('           w%ival(isize) = 0\n')

            [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",descr,"standard")
            indent="           "
            indent = start_loop_i1234(ff,indent,idloop)
            ff.write(indent + '   IF((ss%' + name + dstr + '.NE.0)' +
                           '.AND.(ss%' + name + dstr + '.NE.ps_unInit)) THEN\n')
            ff.write(indent + '      w%ival(isize) = 1\n')
            ff.write(indent + '      exit\n')
            ff.write(indent + '   ENDIF\n')
            indent = end_loop_i1234(ff,indent,idloop)
        else:
            ff.write('           w%ival(isize) = 0\n')

            [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",descr,"standard")
            indent="           "
            indent = start_loop_i1234(ff,indent,idloop)
            ff.write(indent + '   IF(ss%' + name + dstr + '.NE." ") THEN\n')
            ff.write(indent + '      w%ival(isize) = 1\n')
            ff.write(indent + '      exit\n')
            ff.write(indent + '   ENDIF\n')
            indent = end_loop_i1234(ff,indent,idloop)

        ff.write('        ELSE\n')
        ff.write('           w%ival(isize) = -1\n')
        ff.write('        ENDIF\n')

def cmt_dcmt(key,cmt):

    # enclose a description string safely in quotes
    # choose single quote delimiter if string contains double quotes
    # choose double quote delimiter if string contains single quotes
    # if it contains both convert dbl->single and print warning

    if '"' not in cmt:
        dcmt = '"' + cmt + '"'

    elif "'" not in cmt:
        dcmt = "'" + cmt + "'"

    else:
        dcmt = ""
        for char in cmt:
            if char == '"':
                dcmt = dcmt + "'"
            else:
                dcmt = dcmt + char

        print("  description for " + key + ": " + cmt)
        print("  converted to fortran character string: " + dcmt)

    return dcmt

def wr_contents_clause(f,psc,typeval,name,ctrl):

    # write clause for ps_contents_alloc.f90

    if ctrl != "START" and ctrl != "FINISH" and ctrl != "BOTH":
        print("wr_contents_clause: internal error, unexpected ctrl: ",ctrl)
        raise wr_contents_clause_error

    if ctrl == "START" or ctrl == "BOTH":
        f.write("\n")
        f.write("  IF(icomponent(" + psc + ").EQ.1) THEN\n")
        f.write("     IF(itype.EQ." + typeval + ") THEN\n")
        f.write("        isize = isize + 1  ! " + name + "\n")

    if ctrl == "FINISH" or ctrl == "BOTH":
        f.write("     ENDIF\n")
        f.write("  ENDIF\n")

def psw_catfind(section):

    # from section name pick ps_ constant value for "itype" test in code

    sect = section.upper()

    if sect == "MACHINE_DESCRIPTION":
        cat = 'ps_machine_descr'

    elif sect == "SHOT_CONFIGURATION":
        cat = 'ps_shot_config'

    elif sect == "SIMULATION_INIT":
        cat = 'ps_sim_control'

    elif sect == "STATE_DATA":
        cat = 'ps_data'

    elif sect == "STATE_PROFILES":
        cat = 'ps_profiles'

    else:
        print(" ?? psw_catfind internal error: section ID unknown: ",section)
        raise psw_catfind_error

    return cat

def psw_ival_expr(f,dpair):

    # define w%ival(isize) as the array dimension size; possible
    # adjustment due to lower dimension other than one...

    dlower = dpair[0]
    dupper = dpair[1]

    dimsym = "ss%" + dupper

    dincr = 1 - int(dlower)
    if dincr < 0:
        distr = ' ' + repr(dincr)
    else:
        distr = ' +' + repr(dincr)

    if dlower == "1":
        f.write('        w%ival(isize) = ' + dimsym + '\n')
    else:
        f.write('        IF(' + dimsym + '.EQ.0) then\n')
        f.write('           w%ival(isize) = 0\n')
        f.write('        ELSE\n')
        f.write('           w%ival(isize) = ' + dimsym + distr + '\n')
        f.write('        ENDIF\n')

def wr_count_dims(f):

    # write code that counts the number of non-zero dimensions

    f.write('\n')
    f.write('  ss%active_dims = 0\n')

    cclist = list(all_components.keys())
    cclist.sort()

    f.write('\n')
    f.write('  ! count the number of active array dimensions...\n')

    icount = 0
    for cc in cclist:
        f.write('\n')
        f.write('  ! component: ' + cc + ':\n')

        icount = icount + 1
        for dimkey in all_dims:
            idescr = all_items[dimkey]
            dcc = idescr['component']
            if cc == dcc:
                name = idescr['name']
                dimtype = idescr['dimtype']
                str = '  IF(ss%' + name + '.GT.0)'
                str = str + ' ss%active_dims(' + repr(icount)
                str = str + ') = ss%active_dims(' + repr(icount) + ') + 1\n'
                f.write(str)


def id_status(idescr):

    # determine if item (w/passed dictionary) has an associated ID
    # return STANDALONE if it is a standalone profile with an integer scalar
    # ID; return ARRAY if it is a profile array with an allocatable integer
    # array of IDs; return NONE if there are no IDs.

    idstat = "NONE"

    if 'methkey' in idescr:
        methkey = idescr['methkey']

        # Hermite derivative profiles are linked to their parent functions and
        # do not have their own IDs...

        if methkey != "Hermite_derivative":

            dlist = idescr['dlist']
            rank_coord = idescr['rank_coord']

            if rank_coord == len(dlist):
                idstat = "STANDALONE"
            else:
                idstat = "ARRAY"

    return idstat

def plasma_state_write_zonwt():

    # generate plasma_state_zonwt -- routine to set id_zonwt array element
    # which shows identifies weighting of profiles for conservative
    # rezoning...

    print("...plasma_state_write_zonwt()...")


    for key in profile_list:
        descr = all_items[key]

        factor = descr['factor']
        if factor not in ["PS_DV", "PS_DA", " "]:

            name = descr['name']
            id_targ = "id_" + name

            ielems = factor.split(",")
            ilen = len(ielems)
            if ilen > 1:
                # e.g. do: NS(:,PS_ELEC_INDEX) -> ID_NS(PS_ELEC_INDEX)
                icount = 1
                id_name = "id_" + ielems[0][:-2]
                dstr = "("
                # always skip the first element
                for elem in ielems[1:]:
                    icount = icount + 1
                    if icount == ilen:
                        dstr = dstr + elem
                    elif elem != ":":
                        dstr = dstr + elem + ","

                f_zonwt.write("\n")
                f_zonwt.write("  IF(ALLOCATED(ss%" + id_name + ")) THEN\n")
                f_zonwt.write("     IF((ss%" + id_targ + ".GT.0).AND.(ss%" + id_targ + ".LE.isize)) THEN\n")
                f_zonwt.write("        IF((ss%" + id_name + dstr + ".GT.0).AND.(ss%" + id_name + dstr + ".LE.isize)) THEN\n")
                f_zonwt.write("           ss%id_zonwt(ss%" + id_targ + ") = ss%" + id_name + dstr + "\n")
                f_zonwt.write("        ELSE\n")
                f_zonwt.write("           iertot=iertot+1\n")
                f_zonwt.write('           WRITE(iout,*) " ?plasma_state_zonwt_update element: ' + id_name + dstr + ' = ",ss%' + id_name + dstr + '\n')
                f_zonwt.write('           WRITE(iout,*) "  out of bounds 1 to ",isize\n')
                f_zonwt.write("        ENDIF\n")
                f_zonwt.write("     ELSE\n")
                f_zonwt.write("        iertot=iertot+1\n")
                f_zonwt.write('        WRITE(iout,*) " ?plasma_state_zonwt_update element: ' + id_targ + ' = ",ss%' + id_targ + '\n')
                f_zonwt.write('        WRITE(iout,*) "  out of bounds 1 to ",isize\n')
                f_zonwt.write("     ENDIF\n")
                f_zonwt.write("  ENDIF\n")

            else:
                # weighting profile dimensioning matches target dimensioning
                fkey = factor.upper()
                fdescr = all_items[fkey]
                fdlist = fdescr['dlist']
                f_rank = fdescr['rank_coord']

                id_name = "id_" + factor

                if f_rank < len(fdlist):
                    # weighting profile IDs in allocated array
                    [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",descr,"standard")

                    f_zonwt.write("\n")
                    f_zonwt.write("  IF(ALLOCATED(ss%" + id_name + ")) THEN\n")
                    f_zonwt.write("     iertarg = 0; ierval = 0\n")

                    indent = "     "  # indentation of first DO loop
                    indent = start_loop_i1234(f_zonwt,indent,idloop)

                    f_zonwt.write(indent + "   IF((ss%" + id_targ + dstr + ".GT.0).AND.(ss%" + id_targ + dstr + ".LE.isize)) THEN\n")
                    f_zonwt.write(indent + "      IF((ss%" + id_name + dstr + ".GT.0).AND.(ss%" + id_name + dstr + ".LE.isize)) THEN\n")
                    f_zonwt.write(indent + "         ss%id_zonwt(ss%" + id_targ + dstr + ") = ss%" + id_name + dstr + "\n")
                    f_zonwt.write(indent + "      ELSE\n")
                    f_zonwt.write(indent + "         ierval = ierval + 1\n")
                    f_zonwt.write(indent + "      ENDIF\n")
                    f_zonwt.write(indent + "   ELSE\n")
                    f_zonwt.write(indent + "      iertarg = iertarg + 1\n")
                    f_zonwt.write(indent + "   ENDIF\n")

                    indent = end_loop_i1234(f_zonwt,indent,idloop)

                    f_zonwt.write("     IF(iertarg.GT.0) THEN\n")
                    f_zonwt.write('        WRITE(iout,*) " ?plasma_state_zonwt_update target element out of bounds detected!"\n')
                    f_zonwt.write('        write(IOUT,*) "  ' + id_targ + ': ",ss%' + id_targ + '\n')
                    f_zonwt.write('        write(IOUT,*) "  allowable range is 1 to ",isize\n')
                    f_zonwt.write('        iertot = iertot + 1\n')
                    f_zonwt.write("     ENDIF\n")
                    f_zonwt.write("     IF(ierval.GT.0) THEN\n")
                    f_zonwt.write('        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"\n')
                    f_zonwt.write('        write(IOUT,*) "  ' + id_name + ': ",ss%' + id_name + '\n')
                    f_zonwt.write('        write(IOUT,*) "  allowable range is 1 to ",isize\n')
                    f_zonwt.write('        iertot = iertot + 1\n')
                    f_zonwt.write("     ENDIF\n")
                    f_zonwt.write("  ENDIF\n")
                else:
                    # weighting profile & weighted profiles are singletons
                    f_zonwt.write("  !singleton\n")
                    f_zonwt.write("  IF(ss%" + id_targ + ".GT.0) THEN\n")
                    f_zonwt.write("     IF(ss%" + id_targ + ".LE.isize) THEN\n")
                    f_zonwt.write("        IF((ss%" + id_name + ".GT.0).AND.(ss%" + id_name + ".LE.isize)) THEN\n")
                    f_zonwt.write("           ss%id_zonwt(ss%" + id_targ + ") = ss%" + id_name + "\n")
                    f_zonwt.write("        ELSE\n")
                    f_zonwt.write('           WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"\n')
                    f_zonwt.write('           write(IOUT,*) "  ' + id_name + ': ",ss%' + id_name + '\n')
                    f_zonwt.write('           write(IOUT,*) "  allowable range is 1 to ",isize\n')
                    f_zonwt.write('           iertot = iertot + 1\n')
                    f_zonwt.write("        ENDIF\n")
                    f_zonwt.write("     ELSE\n")
                    f_zonwt.write('        WRITE(iout,*) " ?plasma_state_zonwt_update weight element out of bounds detected!"\n')
                    f_zonwt.write('        write(IOUT,*) "  ' + id_targ + ': ",ss%' + id_targ + '\n')
                    f_zonwt.write('        write(IOUT,*) "  allowable range is 1 to ",isize\n')
                    f_zonwt.write('        iertot = iertot + 1\n')
                    f_zonwt.write("     ENDIF\n")
                    f_zonwt.write("  ENDIF\n")

    f_zonwt.write("""

  IF(iertot.GT.0) THEN
     WRITE(iout,*) ' ?plasma_state_zonwt_update: encountered invalid profile IDs!'
     ierr=9999
  ENDIF

END SUBROUTINE plasma_state_zonwt_update
""")

def gen_idloop(prefix,descr,keywd):

    # generate data for nested loops over all non-coordinate dimensions
    # of a profile array

    dlist = descr['dlist']
    inumd = len(dlist)

    if 'rank_coord' in descr:
        rank_coord = descr['rank_coord']
    else:
        rank_coord = 0
    rank_id = inumd - rank_coord
    if keywd == "enum":
        # leave off 1st dimension (enumeration dimension) of array of scalars
        rank_id = rank_id - 1
    elif keywd != "standard":
        print(" ? gen_idloop internal error: unrecognized keywd: ",keywd)
        raise gen_idloop_error

    idloop = []
    dstr = ""
    namvars = []
    namlbls = []
    enum_maxdim = "FALSE"

    if rank_id > 0:

        irank = rank_id
        inum = inumd

        while irank > 0:
            ivar = "I" + repr(irank)  # e.g: I4,I3,I2,I1
            inum = inum-1
            dpair = dlist[inum]
            dlower = dpair[0]
            dupper = prefix + dpair[1]

            dimvar = dpair[1]
            if dimvar[-2:] == "-1":
                dimvar = duvar[:-2]
            dkey = dimvar.upper()
            ddescr = all_items[dkey]

            if 'name_var' in ddescr:
                namvar = ddescr['name_var']
                namlbl = ddescr['dimlabel']
            else:
                #  no name variable associated with enumeration dimension
                mdkey = dkey
                if mdkey not in list(all_enum_dims.keys()):
                    mdkey = dkey[4:]  # strip off "MAX_"
                    if mdkey not in list(all_enum_dims.keys()):
                        print(" ?plasma_state_write.py: internal error, no dimension label for: " + dkey)
                        raise plasma_state_python_error
                    enum_maxdim = "TRUE"
                ref = all_enum_dims[mdkey]
                dlower = '1'  # force
                if ref == "(index)":
                    namvar = '(enum)'
                    namlbl = '(enum)'
                else:
                    namvar = '(enum:' + ref.lower() + ')'
                    namlbl = '(enum:' + ref.lower() + ')'

            idloop.append([ivar,dlower,dupper])
            namvars.append(namvar)
            namlbls.append(namlbl)

            irank = irank - 1

        icount = 0
        dstr = "("
        for itriple in idloop:
            icount = icount + 1
            indx = rank_id - icount
            if icount < rank_id:
                dstr = dstr + idloop[indx][0] + ","
            else:
                dstr = dstr + idloop[indx][0] + ")"

    return [idloop,dstr,namvars,namlbls,enum_maxdim]

def start_loop_i1234(f,indent,idloop):

    # generate code
    #    DO I4=a,b
    #       DO I3=c,d ...
    # idloop[] contains for each level [loop_variable,lower_limit,upper_limit]

    indent = indent[:-3]
    for itriple in idloop:
        [ivar,dlower,dupper] = itriple
        indent = indent + "   "
        f.write(indent + "DO " + ivar + " = " + dlower + "," + dupper + "\n")

    return indent

def end_loop_i1234(f,indent,idloop):

    # generate end of nested loop set:
    #      ENDDO   ! i1
    #   ENDDO   ! i2
    #   --etc--

    ilen = len(idloop)
    icount = 0

    while icount < ilen:
        icount = icount + 1
        indx = ilen - icount
        ivar = idloop[indx][0]

        f.write(indent + "ENDDO   ! " + ivar + "\n")
        indent = indent[:-3]

    return indent

def plasma_state_write_prune():

    # gen code to remove grids associated with components being removed
    # from a state instance.

    print("...plasma_state_write_prune()...")

    for grid in grid_items:

        # this loops over grid dimension variables and grid variables;
        # here we want to process grid variables only...

        descr = all_items[grid]
        dtype = descr['dtype']
        if dtype == "R":
            component = descr['component']
            psc = "ps_cnum_" + component

            f_prune.write("\n")
            f_prune.write("  IF(cclist(" + psc + ").eq.0) THEN\n")
            str = 'CALL ps_remove_grid(ss%iobj,' + psc + ',"' + grid + '",iout,iertmp)'
            f_prune.write("     " + str + "\n")
            f_prune.write("     ierr=MAX(ierr,iertmp)\n")
            f_prune.write("  ENDIF\n")

    f_prune.write("""

END SUBROUTINE plasma_state_prune_grids
""")

def plasma_state_write_hash_code():

    # generate routine to compute hash codes
    # (for detecting changes in parts of the plasma state).

    # see also "plasma_state_write_hash_diff():"

    print('...plasma_state_write_hash_code()...')

    #--------------------------------------
    # note on this procedure: it writes to TWO sources, open on file
    # handles "f_hashc" and "f_ahash"...
    #--------------------------------------

    aclist = list(all_components.keys())
    aclist.sort()

    f_hashc.write('\n')
    f_hashc.write('  !--------------------------------------------\n')
    f_hashc.write('  ! section hash codes\n')

    f_ahash.write('\n')
    f_ahash.write('  !--------------------------------------------\n')
    f_ahash.write('  ! section hash codes\n')

    icount = 0
    for section in all_sections:
        secname = section.lower()
        f_hashc.write('\n')
        f_hashc.write('  !----------------------\n')
        f_hashc.write('  !  section: ' + section + '\n')

        if section == "MACHINE_DESCRIPTION":
            mylist = mdescr
        elif section == "SHOT_CONFIGURATION":
            mylist = sconfig
        elif section == "SIMULATION_INIT":
            mylist = simcon
        elif section == "STATE_DATA":
            mylist = state_data_list
        elif section == "STATE_PROFILES":
            mylist = []

        if section != "STATE_PROFILES":
            for item in mylist:
                if item not in all_dims:
                    sdescr = all_items[item]
                    name = sdescr['name']
                    component = sdescr['component']
                    f_hashc.write('\n')
                    f_hashc.write('  if(cclist(ps_cnum_' + component + ').eq.ps_chkAll) THEN\n')
                    f_hashc.write('     iaddr1 = ps_shash_' + name + '\n')
                    f_hashc.write('     iaddr2 = iaddr1 + 1\n')

                    psw_section_hash(section,name,mylist)
                    f_hashc.write('     ss%new_hash_codes(iaddr1) = kf1\n')
                    f_hashc.write('     ss%new_hash_codes(iaddr2) = kf2\n')
                    f_hashc.write('  ENDIF\n')

            f_hashc.write('\n')
            f_hashc.write('  IF(icheck_all) THEN\n')
            f_hashc.write('     iaddr1 = ps_shash_' + secname + '\n')
            f_hashc.write('     iaddr2 = iaddr1 + 1\n')

            psw_section_hash(section,"*",mylist)
            f_hashc.write('     ss%new_hash_codes(iaddr1) = kf1\n')
            f_hashc.write('     ss%new_hash_codes(iaddr2) = kf2\n')
            f_hashc.write('  ENDIF\n')

        else:
            f_hashc.write('\n')
            f_hashc.write('  iaddr1 = ps_shash_' + secname + '\n')
            f_hashc.write('  iaddr2 = iaddr1 + 1\n')
            f_hashc.write('  ss%new_hash_codes(iaddr1) = 0\n')
            f_hashc.write('  ss%new_hash_codes(iaddr2) = 0\n')
        f_hashc.write('\n')
        psw_hash_lock_sect("",section)
        f_hashc.write('  CALL chk_hash("SECTION","' + secname + '")\n')

        f_ahash.write('\n')
        f_ahash.write('  IF((htypu.EQ."SECTION").AND.(hnamu.EQ."' +
                      section + '")) THEN\n')
        f_ahash.write('     iaddr = ps_shash_' + secname + '\n')
        f_ahash.write('     RETURN\n')
        f_ahash.write('  ENDIF\n')

    f_hashc.write('\n')
    f_hashc.write('  !--------------------------------------------\n')
    f_hashc.write('  ! dimension hash codes\n')
    f_hashc.write('\n')
    f_hashc.write('1000 continue  ! jump here to skip scalar hashes\n')

    f_ahash.write('\n')
    f_ahash.write('  !--------------------------------------------\n')
    f_ahash.write('  ! dimension hash codes\n')

    for dim in all_dims:
        descr = all_items[dim]
        component = descr['component']
        name = dim.lower()
        f_hashc.write('\n')
        f_hashc.write('  !----------------------\n')
        f_hashc.write('  ! dimension: ' + dim + '\n')
        f_hashc.write('  IF(cclist(ps_cnum_' + component + ').GE.ps_chkAll) THEN\n')
        f_hashc.write('     iaddr1 = ps_dhash_' + name + '\n')
        f_hashc.write('     iaddr2 = iaddr1 + 1\n')
        psw_dim_hash(dim)
        f_hashc.write('     ss%new_hash_codes(iaddr1) = kf1\n')
        f_hashc.write('     ss%new_hash_codes(iaddr2) = kf2\n')
        f_hashc.write('\n')
        f_hashc.write('     CALL chk_hash("DIMENSION","' + name + '")\n')
        f_hashc.write('  ENDIF\n')

        f_ahash.write('\n')
        f_ahash.write('  IF((htypu.EQ."DIMENSION").AND.(hnamu.EQ."' +
                      dim + '")) THEN\n')
        f_ahash.write('     iaddr = ps_dhash_' + name + '\n')
        f_ahash.write('     RETURN\n')
        f_ahash.write('  ENDIF\n')

    f_hashc.write('\n')
    f_hashc.write('  !--------------------------------------------\n')
    f_hashc.write('  ! profile hash codes\n')

    f_ahash.write('\n')
    f_ahash.write('  !--------------------------------------------\n')
    f_ahash.write('  ! profile hash codes\n')

    for prof in all_profiles:
        descr = all_items[prof]
        component = descr['component']
        name = prof.lower()
        f_hashc.write('\n')
        f_hashc.write('  !----------------------\n')
        f_hashc.write('  ! profile: ' + prof + '\n')
        f_hashc.write('  IF(cclist(ps_cnum_' + component + ').GE.ps_chkAll) THEN\n')
        f_hashc.write('     iaddr1 = ps_phash_' + name + '\n')
        f_hashc.write('     iaddr2 = iaddr1 + 1\n')
        psw_prof_hash(prof)
        f_hashc.write('     ss%new_hash_codes(iaddr1) = kf1\n')
        f_hashc.write('     ss%new_hash_codes(iaddr2) = kf2\n')
        f_hashc.write('\n')
        f_hashc.write('  ENDIF\n')

        f_ahash.write('\n')
        f_ahash.write('  IF((htypu.EQ."PROFILE").AND.(hnamu.EQ."' +
                      prof + '")) THEN\n')
        f_ahash.write('     iaddr = ps_phash_' + name + '\n')
        f_ahash.write('     RETURN\n')
        f_ahash.write('  ENDIF\n')

    f_hashc.write("""
  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?plasma_state_hash_code: change lock violation(s)."
     ierr=9999
  ELSE
     ierr = 0
  ENDIF

  CONTAINS
     SUBROUTINE chk_hash(htype,hname)
        CHARACTER*(*), INTENT(in) :: htype
        CHARACTER*(*), INTENT(in) :: hname

        integer :: iwarn
        logical :: ichange,plasma_state_hash_match

        ichange = .not.plasma_state_hash_match(ss,iaddr1,ierr)

        iwarn = 0
        IF(ichange) THEN
           IF(ilock.EQ.1) THEN
              iwarn = 1
           ELSE IF(ilock.EQ.2) THEN
              iwarn = 2
              iertot = iertot + 1
           ENDIF
        ENDIF

        IF(iwarn.EQ.1) THEN
           WRITE(iout,*) &
                ' %hash code monitored change detected: type="'// &
                trim(htype)//'" name="'//trim(hname)//'".'
        ELSE IF(iwarn.EQ.2) THEN
           WRITE(iout,*) &
                ' ?hash code change LOCK violation: type="'// &
                trim(htype)//'" name="'//trim(hname)//'".'
        ENDIF
     END SUBROUTINE chk_hash

END SUBROUTINE plasma_state_hash_code
""")
    f_ahash.write("""
  WRITE(iout,*) ' %plasma_state_hash_addr: no address found for:'
  WRITE(iout,*) '  htype = "'//trim(htypu)//'"; hname = "'//trim(hnamu)//'".'

  iaddr = 0

END SUBROUTINE plasma_state_hash_addr
""")

def psw_section_hash(section,sname,alist):

    # section hash code -- variables not directly associated with array
    # dimensions...

    if sname != "*":
        akey=sname.upper()
        adescr = all_items[akey]
        avar = adescr['name']
        if 'dlist' in adescr:
            f_hashc.write('     IF(ALLOCATED(ss%' + avar + ')) THEN\n')
            f_hashc.write('        kf1 = 17; kf2 = 23\n')
            f_hashc.write('        call hashCode(ss%' + avar + ',kf1,kf2)\n')
            f_hashc.write('     ELSE\n')
            f_hashc.write('        kf1 = 0; kf2 = 0\n')
            f_hashc.write('     ENDIF\n')
        else:
            f_hashc.write('     kf1 = 17; kf2 = 23\n')
            f_hashc.write('     call hashCode(ss%' + avar + ',kf1,kf2)\n')
    else:
        f_hashc.write('     kf1 = 17; kf2 = 23\n')
        for avar in alist:
            akey = avar.upper()
            adescr = all_items[akey]
            if 'dlist' in adescr:
                f_hashc.write('     IF(ALLOCATED(ss%' + avar + ')) THEN\n')
                f_hashc.write('        call hashCode(ss%' + avar + ',kf1,kf2)\n')
                f_hashc.write('     ENDIF\n')
            else:
                f_hashc.write('     call hashCode(ss%' + avar + ',kf1,kf2)\n')
    f_hashc.write('\n')

def psw_dim_hash(dim):

    # dimension hash code -- 2

    descr = all_items[dim]
    dimtype = descr['dimtype']
    section = descr['section']
    if dimtype in all_coords:
        gridname = descr['gridname']
        f_hashc.write('     IF(ALLOCATED(ss%' + gridname + ')) THEN\n')
        f_hashc.write('        IF(MAXVAL(ABS(ss%' + gridname + ')).EQ.0.0_rspec) THEN\n')
        f_hashc.write('           kf1 = -1; kf2 = -1\n')
        f_hashc.write('        ELSE\n')
        f_hashc.write('           kf1 = 17; kf2 = 23\n')
        f_hashc.write('           call hashCode(ss%' + gridname + ',kf1,kf2)\n')
        f_hashc.write('        ENDIF\n')
        f_hashc.write('     ELSE\n')
        f_hashc.write('        kf1 = ps_unInit; kf2 = ps_unInit\n')
        f_hashc.write('     ENDIF\n')
        f_hashc.write('\n')

    elif dimtype == 'I' or dimtype == 'MAX':
        f_hashc.write('     kf1 = ps_unInit; kf2 = ps_unInit\n')
        dimexpr = 'ss%' + dim
        indent = '     '

        f_hashc.write(indent + 'IF(' + dimexpr + '.NE.0) THEN\n')
        f_hashc.write(indent + '   kf1 = 17; kf2 = 23\n')
        f_hashc.write(indent + '   call hashCode(ss%' + dim + ',kf1,kf2)\n')
        f_hashc.write(indent + 'ENDIF\n')
        f_hashc.write('\n')

    elif dimtype == 'L':
        f_hashc.write('     IF(ss%' + dim + '.EQ.0) THEN\n')
        f_hashc.write('        kf1 = ps_unInit; kf2 = ps_unInit\n')
        f_hashc.write('     ELSE\n')
        f_hashc.write('        kf1 = 17; kf2 = 23\n')
        avar = descr['name_var']
        f_hashc.write('        call hashCode(ss%' + avar + ',kf1,kf2)\n')
        f_hashc.write('     ENDIF\n')
        f_hashc.write('\n')

    elif dimtype == 'S':
        f_hashc.write('     IF(ss%' + dim + '.EQ.0) THEN\n')
        f_hashc.write('        kf1 = ps_unInit; kf2 = ps_unInit\n')
        f_hashc.write('     ELSE\n')
        f_hashc.write('        kf1 = 17; kf2 = 23\n')
        alist = descr['snames']
        for avar in alist:
            f_hashc.write('        call hashCode(ss%' + avar + ',kf1,kf2)\n')
        f_hashc.write('     ENDIF\n')
        f_hashc.write('\n')

    else:
        print(" ?? plasma_state_hash code: internal error, unexpected dimtype: ",dimtype)
        raise plasma_state_hash_code_error

    #  common for all dimtypes...

    psw_hash_lock_sect("   ",section)

def psw_hash_lock_sect(ihd,section):

    if section == "MACHINE_DESCRIPTION":
        f_hashc.write(ihd + '  ilock = ss%lock_machine_descr\n')
    elif section == "SHOT_CONFIGURATION":
        f_hashc.write(ihd + '  ilock = ss%lock_shot_config\n')
    elif section == "SIMULATION_INIT":
        f_hashc.write(ihd + '  ilock = ss%lock_sim_init\n')
    else:
        f_hashc.write(ihd + '  ilock = ps_unlocked\n')

def psw_hash_lock_save(f,section,action,indent):

    if action == "DECLARE":
        f.write(indent + 'INTEGER ilocka(3)\n')

    elif action == "SAVE":
        if section == "MACHINE_DESCRIPTION":
            f.write(indent + 'ilocka(1) = ss%lock_machine_descr\n')
        elif section == "SHOT_CONFIGURATION":
            f.write(indent + 'ilocka(2) = ss%lock_shot_config\n')
        elif section == "SIMULATION_INIT":
            f.write(indent + 'ilocka(3) = ss%lock_sim_init\n')

    elif action == "RESTORE":
        if section == "MACHINE_DESCRIPTION":
            f.write(indent + 'ss%lock_machine_descr = ilocka(1)\n')
        elif section == "SHOT_CONFIGURATION":
            f.write(indent + 'ss%lock_shot_config = ilocka(2)\n')
        elif section == "SIMULATION_INIT":
            f.write(indent + 'ss%lock_sim_init = ilocka(3)\n')

    elif action == "CLEAR":
        if section == "MACHINE_DESCRIPTION":
            f.write(indent + 'ss%lock_machine_descr = ps_unlocked\n')
        elif section == "SHOT_CONFIGURATION":
            f.write(indent + 'ss%lock_shot_config = ps_unlocked\n')
        elif section == "SIMULATION_INIT":
            f.write(indent + 'ss%lock_sim_init = ps_unlocked\n')

def psw_prof_hash(prof):

    # profile hash code: 2 floating point hashes

    loclist = [prof]
    descr = all_items[prof]
    pname = descr['name']

    f_hashc.write('     IF(ALLOCATED(ss%' + pname + ')) THEN\n')
    f_hashc.write('        IF(MAXVAL(ABS(ss%' + pname + ')).EQ.0.0_rspec) THEN\n')
    f_hashc.write('           kf1 = -1; kf2 = -1\n')
    f_hashc.write('        ELSE\n')
    f_hashc.write('           kf1 = 17; kf2 = 23\n')
    f_hashc.write('           call hashCode(ss%' + pname + ',kf1,kf2)\n')
    f_hashc.write('        ENDIF\n')
    f_hashc.write('     ELSE\n')
    f_hashc.write('        kf1 = ps_unInit; kf2 = ps_unInit\n')
    f_hashc.write('     ENDIF\n')


def plasma_state_write_hash_diff():

    # generate routine that writes a report on hash differences btw two states
    # see also "plasma_state_write_hash_code():"

    print('...plasma_state_write_hash_diff()...')

    f_hashd.write('  !--------------------------------------------\n')
    f_hashd.write('  ! section hash codes\n')

    aclist = list(all_components.keys())
    aclist.sort()

    icount = 0
    for section in all_sections:
        name = section.lower()

        f_hashd.write('\n')
        f_hashd.write('  IF((ss(1:ils).EQ."*").OR.' +
                      '(INDEX("' + section + '",ss(1:ils)).GT.0)) THEN\n')
        f_hashd.write('     icomp = icomp + 1\n')
        f_hashd.write('     iaddr1 = ps_shash_' + name + '\n')
        f_hashd.write('     iaddr2 = iaddr1 + 1\n')

        f_hashd.write('     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &\n')
        f_hashd.write('          (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN\n')
        f_hashd.write('        idiff = idiff + 1\n')
        f_hashd.write('        if(idiff.eq.1) call diff_header\n')
        f_hashd.write('        call diff_report("SECTION","' + section + '")\n')
        f_hashd.write('     ENDIF\n')
        f_hashd.write('  ENDIF\n')

    f_hashd.write('\n')
    f_hashd.write('  !--------------------------------------------\n')
    f_hashd.write('  ! dimension hash codes\n')

    for dim in all_dims:
        name = dim.lower()

        f_hashd.write('\n')
        f_hashd.write('  IF((ss(1:ils).EQ."*").OR.' +
                      '(INDEX("' + dim + '",ss(1:ils)).GT.0)) THEN\n')
        f_hashd.write('     icomp = icomp + 1\n')
        f_hashd.write('     iaddr1 = ps_dhash_' + name + '\n')
        f_hashd.write('     iaddr2 = iaddr1 + 1\n')

        f_hashd.write('     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &\n')
        f_hashd.write('        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN\n')
        f_hashd.write('        idiff = idiff + 1\n')
        f_hashd.write('        if(idiff.eq.1) call diff_header\n')
        f_hashd.write('        call diff_report("DIMENSION","' + dim + '")\n')
        f_hashd.write('     ENDIF\n')
        f_hashd.write('  ENDIF\n')

    f_hashd.write('\n')
    f_hashd.write('  !--------------------------------------------\n')
    f_hashd.write('  ! profile hash codes\n')

    for prof in all_profiles:
        name = prof.lower()

        f_hashd.write('\n')
        f_hashd.write('  IF((ss(1:ils).EQ."*").OR.' +
                      '(INDEX("' + prof + '",ss(1:ils)).GT.0)) THEN\n')
        f_hashd.write('     icomp = icomp + 1\n')
        f_hashd.write('     iaddr1 = ps_phash_' + name + '\n')
        f_hashd.write('     iaddr2 = iaddr1 + 1\n')

        f_hashd.write('     IF((ss1%hash_codes(iaddr1).NE.ss2%hash_codes(iaddr1)).OR. &\n')
        f_hashd.write('        (ss1%hash_codes(iaddr2).NE.ss2%hash_codes(iaddr2))) THEN\n')
        f_hashd.write('        idiff = idiff + 1\n')
        f_hashd.write('        if(idiff.eq.1) call diff_header\n')
        f_hashd.write('        call diff_report("PROFILE","' + prof + '")\n')
        f_hashd.write('     ENDIF\n')
        f_hashd.write('  ENDIF\n')

    f_hashd.write("""
  IF(icomp.EQ.0) THEN
     write(iout,*) ' --> plasma_state_hash_diff: no comparison, sstr="'// &
          trim(sstr)//'".'
  ELSE IF(idiff.EQ.0) THEN
     write(iout,*) ' --> plasma_state_hash_diff: no hash differences between states:'
     write(iout,*) '    "'//trim(lbl1)//'" and "'//trim(lbl2)//'".'
     write(iout,*) '    (sstr="'//trim(sstr)//'"; ',icomp,' hashes compared).'
  ELSE
     write(iout,*) ' --> plasma_state_hash_diff: ',idiff,' hash difference(s) between states:'
     write(iout,*) '    "'//trim(lbl1)//'" and "'//trim(lbl2)//'".'
     write(iout,*) '    (sstr="'//trim(sstr)//'"; ',icomp,' hashes compared).'
  ENDIF
CONTAINS
  subroutine diff_header
     write(iout,*) ' ----------------------- '
     write(iout,*) ' plasma_state_hash_diff detects hash code differences:'
  end subroutine diff_header

  subroutine diff_report(typestr,namestr)
     character*(*) :: typestr
     character*(*) :: namestr
     write(iout,*) '     '//trim(typestr)//' hash for '//trim(namestr)//' is different.'
  end subroutine diff_report

END SUBROUTINE plasma_state_hash_diff
""")

#--------------------------------------------------------------------------
#  plasma_state_write_get
#  Write the subroutine "plasma_state_get" -- used for reading an entire state

def plasma_state_write_get():

    print('...plasma_state_write_get()...')

    #-----------------------------------------------
    # GENERATE CODE

    # list of variables that need extra checking on read: use lowercase!

    chklist = []
    chklist.append("freq_ec")
    chklist.append("freq_ic")
    chklist.append("freq_lh")

    f_xget.write('\n')
    f_xget.write('  ! *** (scalar) Array Dimensions ***\n')
    f_xget.write('  ss%active_dims = 0\n')

    for dim in all_dims:
        descr = all_items[dim]
        name = descr['name']
        component = descr['component']
        dimtype = descr['dimtype']

        f_xget.write('\n')
        f_xget.write('  ss%' + name + ' = 0\n')

        ncdname = gen_dname(0,name)
        f_xget.write('  status = nf90_inq_dimid(icdf,"' + ncdname +
                         '",dimid)\n')
        f_xget.write('  IF(status.EQ.NF90_NOERR) THEN\n')
        f_xget.write('     status = nf90_inquire_dimension(icdf,' +
                     'dimid,len=ss%' + name + ')\n')
        f_xget.write('     IF(status.NE.NF90_NOERR) THEN\n')
        f_xget.write('        ss%' + name + ' = 0\n')
        f_xget.write('        CALL ncerr_name(2,status,"' + name + '")\n')
        f_xget.write('     ENDIF\n')
        f_xget.write('  ENDIF\n')

        f_xget.write('  IF(ss%' + name + '.GT.0) THEN\n')
        dvar = 'ss%active_dims(ps_cnum_' + component + ')'
        f_xget.write('     ' + dvar + ' = ' + dvar + ' + 1\n')
        f_xget.write('  ENDIF\n')

    f_xget.write('\n')
    f_xget.write('  IF(iertot.NE.0) THEN\n')
    f_xget.write('     WRITE(iout,*) " ?plasma_state_get: there were errors reading dimensions..."\n')
    f_xget.write('     ierr=9999\n')
    f_xget.write('     status = nf90_close(icdf)\n')
    f_xget.write('     RETURN\n')
    f_xget.write('  ENDIF\n')

    f_xget.write('\n')
    f_xget.write('  ! DIMENSIONS read in OK; allocate state arrays...\n')
    f_xget.write('\n')
    f_xget.write('  CALL plasma_state_empty_alloc(ss,ierr)\n')
    f_xget.write('  IF(ierr.NE.0) THEN\n')
    f_xget.write('     WRITE(iout,*) " ?plasma_state_get: plasma_state_empty_alloc error."\n')
    f_xget.write('     ierr=9999\n')
    f_xget.write('     status = nf90_close(icdf)\n')
    f_xget.write('     RETURN\n')
    f_xget.write('  ENDIF\n')

    #-----------------
    # state now allocated -- proceed to read non-dimension scalar & array data

    f_xget.write('\n')
    f_xget.write('  !----------------------------------------\n')
    f_xget.write('  ! call subroutines to read variables and arrays\n')
    f_xget.write('\n')
    #    (add calls to plasma_state_get_<nnn> subroutines)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_get_" + repr(isub_count)
        ff = f_xgeta[isub_count]

        arglist = "(ss,icdf,version_change,iout,iertot)\n"
        outcall = "  call " + sname + arglist
        f_xget.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)

        ff.write("""

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

""")
        isub_count = isub_count + 1

    ilist = list(all_items.keys())
    ilist.sort()

    # list of (nsplit_sce) sublists
    isublists = sublist_gen(ilist)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_get_" + repr(isub_count)
        ## print(" **** ",isub_count, isublists[isub_count])
        ff = f_xgeta[isub_count]

        ff.write('\n')
        ff.write("  !-----------------------------------------------\n")
        ff.write("  ! NetCDF reads...\n")

        for item in isublists[isub_count]:

            if item not in all_dims:
                ff.write('\n')
                descr = all_items[item]
                kcat = elem_category(descr)
                name = descr['name']
                init = descr['init']
                if 'dlist' in descr:
                    ff.write('  IF(ALLOCATED(ss%' + name + ')) THEN\n')
                    indent = "     "
                    if kcat == "enum_array":
                        aarg = kcat
                    else:
                        aarg = "array"
                    psw_get1var(ff,indent,name,init,descr,aarg,chklist)
                    ff.write('  ENDIF\n')
                else:
                    indent = "  "
                    psw_get1var(ff,indent,name,init,descr,"scalar",chklist)

        psw_gtrailer(ff,sname)
        isub_count = isub_count + 1

    #-----------------
    # read from  NetCDF complete

    f_xget.write('\n')
    f_xget.write('  !----------------------------------------\n')
    f_xget.write('  ! Close the NetCDF file\n')
    f_xget.write('\n')
    f_xget.write('  status = nf90_close(icdf)\n')

    f_xget.write('\n')
    f_xget.write('  IF(iertot.NE.0) THEN\n')
    f_xget.write('     WRITE(iout,*) " ?plasma_state_get: there were errors reading data..."\n')
    f_xget.write('     ierr=9999\n')
    f_xget.write('     RETURN\n')
    f_xget.write('  ENDIF\n')

    f_xget.write('\n')
    f_xget.write('  !----------------------------------------\n')
    f_xget.write('  ! Store in xplasma; check equilibrium & hash codes\n')
    f_xget.write('\n')

    f_xget.write('  IF(version_change) THEN\n')
    indent = "     "
    for section in all_sections:
        psw_hash_lock_save(f_xget,section,"SAVE",indent)
        psw_hash_lock_save(f_xget,section,"CLEAR",indent)
    f_xget.write('  ENDIF\n')

    f_xget.write('\n')
    f_xget.write('  CALL plasma_state_store(" ",ss,1,ierr)\n')
    f_xget.write('\n')

    f_xget.write('  IF(version_change) THEN\n')
    indent = "     "
    for section in all_sections:
        psw_hash_lock_save(f_xget,section,"RESTORE",indent)
    f_xget.write('  ENDIF\n')

    #----------------------
    # trailing write...

    psw_gtrailer(f_xget,"plasma_state_get")

def psw_gtrailer(ff,sname):

    ff.write("""
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

""")
    ff.write("END SUBROUTINE " + sname + "\n")

def psw_get1var(ff,indent,name,init,descr,loctype,chklist):

    # write code to read one item from NetCDF

    dtype = descr['dtype']

    lowname = name.lower()

    ff.write(indent + 'status = nf90_inq_varid(icdf,"' + name +
                 '",varid)\n')
    ff.write(indent + 'IF(status.NE.NF90_NOERR) THEN\n')
    ff.write(indent + '   CALL ncerr_name(1,status,"' + name + '")\n')
    ff.write(indent + 'ELSE\n')
    if loctype == "scalar" and ( dtype[0:2] == "C*" or dtype == "N" ):
        ff.write(indent + '   ilen = len(ss%' + name + ')\n')
        ff.write(indent +
                     '   status = nf90_inquire_variable(icdf,varid,' +
                     'ndims=indim,dimids=idims)\n')
        ff.write(indent + '   IF((status.EQ.NF90_NOERR).AND.'
                     + '(indim.eq.1)) THEN\n')
        ff.write(indent +
                     '      status = nf90_inquire_dimension(icdf,' +
                     'idims(1),len=jlen)\n')
        ff.write(indent +
                     '      IF(status.EQ.NF90_NOERR) ilen=min(ilen,jlen)\n')
        ff.write(indent + '   ENDIF\n')
        ff.write(indent + '   ss%' + name + ' = ' + init + '\n')
        ff.write(indent + '   status = nf90_get_var(icdf,varid,ss%' +
                     name + '(1:ilen))\n')
    else:

        ff.write(indent + '   status = nf90_get_var(icdf,varid,ss%' +
                     name + ')\n')
    ff.write(indent + '   IF(status.NE.NF90_NOERR) THEN\n')
    ff.write(indent + '      ss%' + name + ' = ' + init + '\n')
    ff.write(indent + '      CALL ncerr_name(2,status,"' + name +
                 '")\n')

    if dtype == "F" or dtype == "N" or dtype[0:2] == "C*":
        ff.write(indent + '   ELSE\n')
        if loctype == "scalar":
            ff.write(indent + '      CALL denull(ss%' + name + ')\n')
        else:
            dlist = descr['dlist']
            dpair = dlist[0]
            dlower = dpair[0]
            dupper = dpair[1]

            incr = gen_size_adjust(dlower)
            isize = dupper + incr

            ff.write(indent + '      CALL denull_vec(ss%' + name +
                         ',ss%' + isize + ')\n')

        if lowname in chklist:
            ff.write(indent + '      CALL ps_varchk(ss,"' + lowname +
                         '",icdf,varid,iout)\n')

    else:
        if lowname in chklist:
            ff.write(indent + '   ELSE\n')
            ff.write(indent + '      CALL ps_varchk(ss,"' + lowname +
                         '",icdf,varid,iout)\n')

    ff.write(indent + '   ENDIF\n')
    ff.write(indent + 'ENDIF\n')

#--------------------------------------------------------------------------
#  plasma_state_write_store
#  Write the subroutine "plasma_state_store" -- used for writing state

def plasma_state_write_store():

    print('...plasma_state_write_store()...')

    #-----------------------------------------------
    # GENERATE CODE

    f_xput.write('\n')
    f_xput.write('  ! *** Array Dimensions ***\n')
    f_xput.write('\n')
    f_xput.write('  ss%active_dims = 0\n')
    f_xput.write('  iertot = 0\n')

    for dim in all_dims:
        ddescr = all_items[dim]
        name = ddescr['name']
        ilist = ddescr['ilist']
        component = ddescr['component']
        dimtype = ddescr['dimtype']

        f_xput.write('\n')
        f_xput.write('  IF(ss%' + name + '.GT.0) THEN\n')

        dvar = 'ss%active_dims(ps_cnum_' + component + ')'
        f_xput.write('     ' + dvar + ' = ' + dvar + ' + 1\n')

        for incr in ilist:
            if incr == 0:
                f_xput.write('     ival = ss%' + name + '\n')
            elif incr < 0:
                f_xput.write('     ival = ss%' + name + ' - ' + repr(-incr) + '\n')
            else:
                f_xput.write('     ival = ss%' + name + ' + ' + repr(incr) + '\n')
            ncdname = gen_dname(incr,name)
            f_xput.write('     status = nf90_def_dim(icdf,"' +
                         ncdname + '",ival,dimid)\n')
            f_xput.write('     IF(status.NE.NF90_NOERR) THEN\n')
            f_xput.write('        write(iout,*) " error defining dimension: '
                         + ncdname + '"\n')
            f_xput.write('        CALL ncerr(status)\n')
            f_xput.write('        iertot = iertot + 1\n')
            f_xput.write('     ENDIF\n')

        f_xput.write('  ENDIF\n')

    for iclen in char_lens:
        f_xput.write('\n')
        name = 'ps_clen_' + repr(iclen)
        f_xput.write('  status = nf90_def_dim(icdf,"' + name + '",'
                     + repr(iclen) + ',dimid)\n')
        f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
        f_xput.write('     write(iout,*) " error defining dimension: '
                     + name + '"\n')
        f_xput.write('     CALL ncerr(status)\n')
        f_xput.write('     iertot = iertot + 1\n')
        f_xput.write('  ENDIF\n')

    # also put out a dimension for the hash codes; defin hash array also

    f_xput.write('\n')
    f_xput.write('  ! hash codes dimension\n')
    f_xput.write('  status = nf90_def_dim(icdf,"ps_hash_dim",' +
                 'ps_hash_total,dimid)\n')
    f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_xput.write('     write(iout,*) " error defining dimension: '
                 + ' ps_hash_dim"\n')
    f_xput.write('     CALL ncerr(status)\n')
    f_xput.write('     iertot = iertot + 1\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  ! hash codes array\n')
    f_xput.write('  idims(1)=dimid\n')
    f_xput.write('  status = nf90_def_var(icdf,"hash_codes",NF90_INT, &\n')
    f_xput.write('            dimids = idims(1:1), varid=varid)\n')
    f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_xput.write('     write(iout,*) " error defining: '
                 + ' hash_codes array variable."\n')
    f_xput.write('     CALL ncerr(status)\n')
    f_xput.write('     iertot = iertot + 1\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  IF(iertot.GT.0) THEN\n')
    f_xput.write('     WRITE(iout,*) " ?plasma_state_store: error defining NetCDF dimensions."\n')
    f_xput.write('     ierr = 9999\n')
    f_xput.write('     status = nf90_close(icdf)\n')
    f_xput.write('     RETURN\n')
    f_xput.write('  ELSE\n')
    f_xput.write('     ierr = 0\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  !-------------------------------------------------\n')
    f_xput.write('  !  define variables with attributes\n')
    f_xput.write('\n')

    #  version_id first

    f_xput.write('\n')
    f_xput.write('  status = nf90_inq_dimid(icdf,"ps_clen_32",idims(1))\n')
    f_xput.write('  status = nf90_def_var(icdf,"version_id",NF90_CHAR, &\n')
    f_xput.write('            dimids = idims(1:1), varid=varid)\n')
    f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_xput.write('     write(iout,*) " error defining: '
                 + ' version_id character string."\n')
    f_xput.write('     CALL ncerr(status)\n')
    f_xput.write('     iertot = iertot + 1\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  !  define variables with attributes -- subroutine calls\n')
    f_xput.write('\n')

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_store_def_" + repr(isub_count)
        ff = f_xputa[isub_count]

        arglist = "(ss,icdf,iout,iertot)\n"
        outcall = "  call " + sname + arglist
        f_xput.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)

        ff.write("""

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

""")
        isub_count = isub_count + 1

    ilist = list(all_items.keys())
    ilist.sort()

    # list of (nsplit_sce) sublists
    isublists = sublist_gen(ilist)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_store_def_" + repr(isub_count)
        ff = f_xputa[isub_count]

        for item in isublists[isub_count]:

            if item not in all_dims:
                ff.write('\n')
                descr = all_items[item]
                kcat = elem_category(descr)

                name = descr['name']
                cmts = descr['cmts']
                cmt = cmts[0]
                dcmt = cmt_dcmt(item,cmt)

                dtype = descr['dtype']
                if 'units' in descr:
                    units = descr['units']
                else:
                    units = "__NONE"

                section = descr['section']
                component = descr['component']
                spec = descr['spec']
                dspec = cmt_dcmt(item,spec)

                if 'dlist' in descr:
                    ff.write('  IF(ALLOCATED(ss%' + name + ')) THEN\n')
                    indent = '     '
                    dlist = descr['dlist']
                else:
                    indent = '  '
                    dlist = []

                gen_dimstart(ff,dtype,indent,1)

                for dpair in dlist:
                    ncdname = gen_dimname(dpair)
                    ff.write(indent + 'indim = indim + 1\n')
                    ff.write(indent + 'status = nf90_inq_dimid(icdf,"'
                             + ncdname + '",idims(indim))\n')

                ff.write(indent +
                         'CALL define_var("' + name + '","' + dtype
                         + '",indim,idims, &\n')
                ff.write(indent + '       ' + dcmt + ', &\n')
                ff.write(indent + '       "' + units + '","' + component
                         + '","' + section + '", &\n')
                ff.write(indent + '       ' + dspec + ')\n')

                if len(dlist) > 0:
                    ff.write('  ENDIF\n')

        psw_wtrailer(ff,1,sname)
        isub_count = isub_count + 1

    f_xput.write('\n')
    f_xput.write('  IF(iertot.GT.0) THEN\n')
    f_xput.write('     WRITE(iout,*) " ?plasma_state_store: error defining NetCDF variables."\n')
    f_xput.write('     ierr = 9999\n')
    f_xput.write('     status = nf90_close(icdf)\n')
    f_xput.write('     RETURN\n')
    f_xput.write('  ELSE\n')
    f_xput.write('     ierr = 0\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  !------------------------------------\n')
    f_xput.write('  ! transfer the data to NetCDF\n')

    f_xput.write('\n')
    f_xput.write('  status = nf90_enddef(icdf)  ! exit definition mode\n')
    f_xput.write('\n')
    f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_xput.write('     WRITE(iout,*) " ?plasma_state_store: nf90_enddef error"\n')
    f_xput.write('     CALL ncerr(status)\n')
    f_xput.write('     ierr = 9999\n')
    f_xput.write('     status = nf90_close(icdf)\n')
    f_xput.write('     RETURN\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  iertot = 0\n')

    f_xput.write('\n')
    f_xput.write('  status = nf90_inq_varid(icdf,"version_id",varid)\n')
    f_xput.write('  status = nf90_put_var(icdf,varid,ps_version_id)\n')
    f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_xput.write('     WRITE(iout,*) " ?plasma_state_store: error storing version_id"\n')
    f_xput.write('     CALL ncerr(status)\n')
    f_xput.write('     ierr = 9999\n')
    f_xput.write('     status = nf90_close(icdf)\n')
    f_xput.write('     RETURN\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  status = nf90_inq_varid(icdf,"hash_codes",varid)\n')
    f_xput.write('  status = nf90_put_var(icdf,varid,ss%hash_codes)\n')
    f_xput.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_xput.write('     WRITE(iout,*) " ?plasma_state_store: error storing hash_codes"\n')
    f_xput.write('     CALL ncerr(status)\n')
    f_xput.write('     ierr = 9999\n')
    f_xput.write('     status = nf90_close(icdf)\n')
    f_xput.write('     RETURN\n')
    f_xput.write('  ENDIF\n')

    f_xput.write('\n')
    f_xput.write('  !  write variables -- subroutine calls\n')
    f_xput.write('\n')

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_store_" + repr(isub_count)
        ff = f_xputa[isub_count]

        arglist = "(ss,icdf,iout,iertot)\n"
        outcall = "  call " + sname + arglist
        f_xput.write(outcall)

        sdecl = "\n\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)

        ff.write("""

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

""")
        isub_count = isub_count + 1

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "plasma_state_store_" + repr(isub_count)
        ff = f_xputa[isub_count]

        for item in isublists[isub_count]:

            if item not in all_dims:
                ff.write('\n')

                descr = all_items[item]
                name = descr['name']
                kcat = elem_category(descr)

                if 'dlist' in descr:
                    ff.write('  IF(ALLOCATED(ss%' + name + ')) THEN\n')
                    indent = '     '
                else:
                    indent = '  '

                ff.write(indent + 'status = nf90_inq_varid(icdf,"' + name + '",varid)\n')
                ff.write(indent + 'status = nf90_put_var(icdf,varid,ss%' + name + ')\n')
                ff.write(indent + 'IF(status.NE.NF90_NOERR) THEN\n')
                ff.write(indent + '   WRITE(iout,*) " ?plasma_state_store: error storing: ' + name + '"\n')
                ff.write(indent + '   CALL ncerr(status)\n')
                ff.write(indent + '   iertot = iertot + 1\n')
                ff.write(indent + 'ENDIF\n')

                if 'dlist' in descr:
                    ff.write('  ENDIF\n')

        psw_wtrailer(ff,2,sname)
        isub_count = isub_count + 1

    f_xput.write('\n')
    f_xput.write('  status = nf90_close(icdf)\n')

    f_xput.write('\n')
    f_xput.write('  IF(iertot.GT.0) THEN\n')
    f_xput.write('     WRITE(iout,*) " ?plasma_state_store: error storing NetCDF variables."\n')
    f_xput.write('     ierr = 9999\n')
    f_xput.write('     RETURN\n')
    f_xput.write('  ELSE\n')
    f_xput.write('     ierr = 0\n')
    f_xput.write('  ENDIF\n')

    # trailer code
    psw_wtrailer(f_xput,2,"plasma_state_store")

def psw_wtrailer(ff,iopt,sname):

    ff.write("\n  CONTAINS\n\n")

    if iopt == 1:
        ff.write("""
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
""")
    ff.write("""
     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

""")
    ff.write("END SUBROUTINE " + sname + "\n")

def gen_dimname(dpair):

    # generate NetCDF dimension name from dimension limit pair

    dlower = dpair[0]
    dupper = dpair[1]

    incr = 0
    if dlower != "I" and dlower != "NEW":
        incr = 1 - int(dlower)

    if dupper[-2:] == "-1":
        incr = incr - 1
        dname = dupper[:-2]
    else:
        dname = dupper

    ncdname = gen_dname(incr,dname)
    return ncdname

def gen_dname(incr,name):

    # generate NetCDF dimension name from state dimension variable name

    if incr == 0:
        ncdname = "dim_" + name.lower()
    elif incr < 0:
        ncdname = "dm" + repr(-incr) + "_" + name.lower()
    else:
        ncdname = "dp" + repr(incr) + "_" + name.lower()

    return ncdname

def gen_dimstart(f,dtype,indent,icall):

    # generate start of dimensioning information for NetCDF variable
    # definition
    #   dtype is type of variable; indent is indentation level for output

    if dtype == "N":
        f.write(indent + 'indim = 1\n')
        if icall == 1:
            dname = 'ps_clen_32'
            f.write(indent + 'status = nf90_inq_dimid(icdf,"' + dname
                    + '",idims(1))\n')
        else:
            f.write(indent + 'idims(1)=nids%nid_clen_32\n')

    elif dtype == "F":
        f.write(indent + 'indim = 1\n')
        if icall == 1:
            dname = 'ps_clen_256'
            f.write(indent + 'status = nf90_inq_dimid(icdf,"' + dname
                    + '",idims(1))\n')
        else:
            f.write(indent + 'idims(1)=nids%nid_clen_256\n')

    elif dtype[0] == "C":
        f.write(indent + 'indim = 1\n')
        if icall == 1:
            dname = 'ps_clen_' + dtype[2:]   # nn of C*nn
            f.write(indent + 'status = nf90_inq_dimid(icdf,"' + dname
                    + '",idims(1))\n')
        else:
            f.write(indent + 'idims(1)=nids%nid_clen_' + dtype[2:] + '\n')


    elif dtype == "R" or dtype == "I":
        f.write(indent + 'indim = 0\n')

    else:
        print("plasma_state_write(gen_dimstart): unexpected dtype: " + dtype)
        raise plasma_state_write_store_error

def plasma_state_write_upwr():

    print('...plasma_state_write_upwr()...')

    #-----------------------------------------------
    # GENERATE CODE: write NetCDF state update file

    # character dimensions -- always written

    f_upwr.write('  iertot = 0\n')
    for iclen in char_lens:
        f_upwr.write('\n')
        name = 'ps_clen_' + repr(iclen)
        nname = 'nids%nid_clen_' + repr(iclen)
        f_upwr.write('  status = nf90_def_dim(icdf,"' + name + '",'
                     + repr(iclen) + ',dimid)\n')
        f_upwr.write('  IF(status.NE.NF90_NOERR) THEN\n')
        f_upwr.write('     WRITE(iout,*) " error defining dimension: '
                     + name + '"\n')
        f_upwr.write('     CALL ncerr(status)\n')
        f_upwr.write('     iertot = iertot + 1\n')
        f_upwr.write('  ELSE\n')
        f_upwr.write('     ' + nname + ' = dimid\n')
        f_upwr.write('  ENDIF\n')

    f_upwr.write("""
  IF(iertot.GT.0) THEN
     WRITE(iout,*) ' ?ps_update_file_write: character length dimension error.'
     ierr=9999
     RETURN
  ENDIF

  idims(1) = nids%nid_clen_32
  status = nf90_def_var(icdf,"version_id",NF90_CHAR, &
           dimids=idims(1:1), varid=vvarid)
  IF(status.NE.NF90_NOERR) THEN
     write(iout,*) " error defining: version_id character string."
     call ncerr(status)
     iertot = iertot + 1
  ENDIF
""")
    f_upwr.write('\n')
    f_upwr.write('  !---------------------------------------------\n')
    f_upwr.write('  ! call subroutines to define data and arrays...\n')
    f_upwr.write('\n')

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ff = f_upwra[isub_count]
        sname = "ps_update_file_write_def_" + repr(isub_count)

        arglist = "(ss,icdf,cclist,nids,iout,iwarn,iertot)\n"
        outcall = "  call " + sname + arglist
        f_upwr.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)
        ff.write("""

  ! Define updated data

  USE plasma_state_definition_mod
  USE netcdf
  USE ps_nidlist_defn
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: cclist(ps_ccount)  ! component selection
  TYPE (nidlist) :: nids       ! defined dimensions list
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iwarn   ! warn count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------
  LOGICAL :: plasma_state_hash_match,ichange

  INTEGER :: varid,pvarid,vvarid,dimid,status,iertmp,iaddr
  INTEGER :: idims(20),indim,ival,ii,ierr
  CHARACTER*80 :: nc_errmsg

  !-------------------

  ierr = 0   ! local
""")

        isub_count = isub_count + 1

    iilist = list(all_items.keys())
    iilist.sort()

    # list of (nsplit_sce) sublists
    isublists = sublist_gen(iilist)

    isub_count = 0
    while ( isub_count < nsplit_sce ):

        ff = f_upwra[isub_count]
        sname = "ps_update_file_write_def_" + repr(isub_count)

        for item in isublists[isub_count]:

            if item not in all_dims:
                ff.write('\n')
                descr = all_items[item]
                kcat = elem_category(descr)

                name = descr['name']
                cmts = descr['cmts']
                cmt = cmts[0]
                dcmt = cmt_dcmt(item,cmt)

                dtype = descr['dtype']
                if 'units' in descr:
                    units = descr['units']
                else:
                    units = "__NONE"

                section = descr['section']
                component = descr['component']

                spec = descr['spec']
                dspec = cmt_dcmt(item,spec)

                dimhash = 0
                if item in grid_items:
                    dimhash = 1
                elif item in species_list_items:
                    dimhash = 1
                elif item in item_list_items:
                    dimhash = 1
                elif item in all_dims:
                    dimhash = 1

                if dimhash == 0:
                    dimhash = ""
                else:
                    dlist = descr['dlist']
                    dpair = dlist[0]
                    dimhash = dpair[1]

                #---------
                #  DMC 2/2010: update report generator code (f_uprp.write(...))

                if section == "STATE_PROFILES":
                    itest = "1"
                elif section == "STATE_DATA":
                    itest = "2"
                else:
                    itest = "3"

                f_uprp.write('\n  IF((cclist(ps_cnum_' + component +
                             ').EQ.1).AND.(jsect.GE.' + itest + ')) THEN\n')

                if section == "STATE_PROFILES":
                    ff.write('  iaddr = ps_phash_' + name + '\n')
                    f_uprp.write('     iaddr = ps_phash_' + name + '\n')
                elif dimhash != "":
                    ff.write('  iaddr = ps_dhash_' + dimhash + '\n')
                    f_uprp.write('     iaddr = ps_dhash_' + dimhash + '\n')
                else:
                    ff.write('  iaddr = ps_shash_' + name + '\n')
                    f_uprp.write('     iaddr = ps_shash_' + name + '\n')

                ff.write('  ichange = .NOT.plasma_state_hash_match(ss,iaddr,iertmp)\n')
                f_uprp.write('     iwrite = plasma_state_hash_match(ss,iaddr,iertmp)\n')
                f_uprp.write('     if(ireverse.NE.1) iwrite = .NOT.iwrite\n')
                f_uprp.write('     if(iwrite) then\n')
                f_uprp.write('        write(iout,"(1x,A)") trim(hdr)//"  ' +
                             name + ' (' + component + ', ' + section + ')"\n')
                f_uprp.write('     endif\n')
                f_uprp.write('  endif\n')

                if 'dlist' in descr:
                    ff.write('  IF(ALLOCATED(ss%' + name + ').AND.ichange) THEN\n')
                    indent = '        '
                    dlist = descr['dlist']
                else:
                    ff.write('  IF(ichange) THEN\n')
                    indent = '        '
                    dlist = []

                ff.write('     IF(cclist(ps_cnum_' + component +
                         ').NE.1) THEN\n')
                ff.write('        CALL ccwarn("' + name + '","' + component +
                         '")\n')
                ff.write('     ELSE\n')

                gen_dimstart(ff,dtype,indent,2)

                for dpair in dlist:
                    ncdname = gen_dimname(dpair)
                    dname = ncdname[4:]
                    if ncdname[0:3] == "dim":
                        dexpr = "ss%" + dname
                    elif ncdname[0:3] == "dm1":
                        dexpr = "ss%" + dname + "-1"
                    elif ncdname[0:3] == "dp1":
                        dexpr = "ss%" + dname + "+1"
                    ff.write('        IF(nids%nid_' + ncdname + '.EQ.0) THEN\n')
                    ff.write('           CALL define_dim(nids%nid_' + ncdname +
                             ',"' + ncdname + '",' + dexpr + ')\n')
                    ff.write('           IF(ierr.NE.0) THEN\n')
                    ff.write('               iertot=iertot+1\n')
                    ff.write('               RETURN\n')
                    ff.write('           ENDIF\n')
                    ff.write('        ENDIF\n')
                    ff.write('        indim = indim + 1\n')
                    ff.write('        idims(indim) = nids%nid_' + ncdname + '\n')

                ff.write('        CALL define_var("' + name + '","' + dtype
                         + '",indim,idims, &\n')
                ff.write('               ' + dcmt + ', &\n')
                ff.write('               "' + units + '","' + component
                         + '","' + section + '", &\n')
                ff.write('               ' + dspec + ')\n')

                ff.write('     ENDIF\n')
                ff.write('  ENDIF\n')

        upwr_trailer(ff,2,sname)
        isub_count = isub_count + 1

    f_upwr.write('\n')
    f_upwr.write('  IF(iertot.GT.0) THEN\n')
    f_upwr.write('     WRITE(iout,*) " ?ps_update_file_write: error defining NetCDF variables."\n')
    f_upwr.write('     ierr = 9999\n')
    f_upwr.write('     status = nf90_close(icdf)\n')
    f_upwr.write('     RETURN\n')
    f_upwr.write('  ELSE\n')
    f_upwr.write('     ierr = 0\n')
    f_upwr.write('  ENDIF\n')

    f_upwr.write('\n')
    f_upwr.write('  !------------------------------------\n')
    f_upwr.write('  ! transfer the data to NetCDF\n')

    f_upwr.write('\n')
    f_upwr.write('  status = nf90_enddef(icdf)  ! exit definition mode\n')
    f_upwr.write('\n')
    f_upwr.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_upwr.write('     WRITE(iout,*) " ?ps_update_file_write: nf90_enddef error"\n')
    f_upwr.write('     CALL ncerr(status)\n')
    f_upwr.write('     ierr = 9999\n')
    f_upwr.write('     status = nf90_close(icdf)\n')
    f_upwr.write('     RETURN\n')
    f_upwr.write('  ENDIF\n')

    f_upwr.write('\n')
    f_upwr.write('  iertot = 0\n')

    f_upwr.write('\n')
    f_upwr.write('  status = nf90_put_var(icdf,vvarid,ps_version_id)\n')
    f_upwr.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_upwr.write('     WRITE(iout,*) " ?ps_update_file_write: error storing ps_version_id"\n')
    f_upwr.write('     CALL ncerr(status)\n')
    f_upwr.write('     ierr = 9999\n')
    f_upwr.write('     status = nf90_close(icdf)\n')
    f_upwr.write('     RETURN\n')
    f_upwr.write('  ENDIF\n')

    f_upwr.write('\n')
    f_upwr.write('  ival = 1\n')
    f_upwr.write('  status = nf90_put_var(icdf,pvarid,ival)\n')
    f_upwr.write('  IF(status.NE.NF90_NOERR) THEN\n')
    f_upwr.write('     WRITE(iout,*) " ?ps_update_file_write: error storing ps_partial_update marker"\n')
    f_upwr.write('     CALL ncerr(status)\n')
    f_upwr.write('     ierr = 9999\n')
    f_upwr.write('     status = nf90_close(icdf)\n')
    f_upwr.write('     RETURN\n')
    f_upwr.write('  ENDIF\n')

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ff = f_upwra[isub_count]
        sname = "ps_update_file_write_" + repr(isub_count)

        arglist = "(ss,icdf,cclist,iucount,iout,iwarn,iertot)\n"
        outcall = "  call " + sname + arglist
        f_upwr.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)
        ff.write("""

  ! Write updated data

  USE plasma_state_definition_mod
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: cclist(ps_ccount)  ! component selection
  INTEGER, INTENT(INOUT) :: iucount ! update count
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iwarn   ! warn count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------

  INTEGER :: varid,status
  CHARACTER*80 :: nc_errmsg

  !-------------------

""")

        isub_count = isub_count + 1

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ff = f_upwra[isub_count]
        sname = "ps_update_file_write_" + repr(isub_count)

        for item in isublists[isub_count]:
            if item not in all_dims:

                ff.write('\n')

                descr = all_items[item]
                kcat = elem_category(descr)
                name = descr['name']

                ff.write('  status = nf90_inq_varid(icdf,"' + name + '",varid)\n')
                ff.write('  IF(status.EQ.NF90_NOERR) THEN\n')
                ff.write('     status = nf90_put_var(icdf,varid,ss%' + name + ')\n')
                ff.write('     IF(status.NE.NF90_NOERR) THEN\n')
                ff.write('        WRITE(iout,*) " ?ps_update_file_write:  error storing updated quantity: ' + name + '"\n')
                ff.write('        CALL ncerr(status)\n')
                ff.write('        iertot = iertot + 1\n')
                ff.write('     ELSE\n')
                ff.write('        iucount = iucount + 1\n')
                ff.write('     ENDIF\n')
                ff.write('  ENDIF\n')

        upwr_trailer(ff,3,sname)
        isub_count = isub_count + 1

    # write loop completed...

    # trailer code
    f_uprp.write("""

END SUBROUTINE ps_update_report_exec

""")

    f_upwr.write("""

  status = nf90_close(icdf)

  IF(iertot.GT.0) THEN
     WRITE(iout,*) " ?ps_update_file_write: error storing NetCDF variables."
     ierr = 9999
     RETURN
  ELSE
     ierr = 0
     WRITE(iout,*) " *ps_update_file_write: normal exit, ",iucount, &
          " update elements written."
  ENDIF

""")
    upwr_trailer(f_upwr,1,"ps_update_file_write")

def upwr_trailer(ff,iopt,sname):

    ff.write("  CONTAINS\n")

    if iopt < 3:
        ff.write("""

     SUBROUTINE define_dim(dimid,dname,idval)
        INTEGER, intent(out) :: dimid
        CHARACTER*(*), INTENT(in) :: dname
        INTEGER, intent(in) :: idval

        status = nf90_def_dim(icdf,dname,idval,dimid)
        IF(status.NE.NF90_NOERR) THEN
           WRITE(iout,*) ' ?ps_update_file_write: error defining dimension "'// &
                 trim(dname)//'".'
           IERR=9999
        ENDIF
     END SUBROUTINE define_dim

""")
    if iopt == 2:
        ff.write("""

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

""")

    ff.write("""

     SUBROUTINE ncerr(status)
        INTEGER, INTENT(in) :: status

        nc_errmsg = NF90_strerror(status)
        write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
     END SUBROUTINE ncerr

""")

    if iopt < 3:
        ff.write("""

     SUBROUTINE ccwarn(varnam,compnam)

        CHARACTER*(*), intent(in) :: varnam   ! name of variable or array
        CHARACTER*(*), intent(in) :: compnam  ! name of component

        if(iwarn.eq.0) then
           write(iout,*) ' '
           write(iout,*) ' %ps_update_file_write: modified state elements'
           write(iout,*) '  exist which were not written due to the "cclist"'
           write(iout,*) '  input variable which screens out elements from'
           write(iout,*) '  selected components: cclist = ',cclist,';'
           write(iout,*) '  element values of 1 indicate components that are'
           write(iout,*) '  enabled for write.'
           write(iout,*) ' '
        endif

        iwarn = iwarn + 1

        write(iout,*) ' %ps_update_file_write: modified: ss%'// &
             trim(varnam)//' ('//trim(compnam)//' component) NOT WRITTEN.'

     END SUBROUTINE ccwarn

""")

    ff.write("END SUBROUTINE " + sname + "\n")


def plasma_state_write_uprd():

    print('...plasma_state_write_uprd()...')

    #-----------------------------------------------
    # GENERATE CODE: write NetCDF state update file

    f_uprd.write('\n')
    f_uprd.write('  ! *** Array Dimensions ***\n')
    f_uprd.write('  icount = 0\n')
    f_uprd.write('  iertot = 0\n')

    for dim in all_dims:
        descr = all_items[dim]
        name = descr['name']

        ilist = descr['ilist']
        f_uprd.write('\n')

        for incr in ilist:
            ncdname = gen_dname(incr,name)
            f_uprd.write('  status = nf90_inq_dimid(icdf,"' + ncdname +
                         '",dimid)\n')
            f_uprd.write('  IF(status.EQ.NF90_NOERR) THEN\n')
            f_uprd.write('     status = nf90_inquire_dimension(icdf,dimid,len=ival)\n')
            if incr < 0:
                f_uprd.write('     ival = ival + ' + repr(-incr) + '\n')

            if incr > 0:
                f_uprd.write('     ival = ival - ' + repr(incr) + '\n')

            f_uprd.write('     IF(ss%' + name + '.EQ.0) THEN\n')
            f_uprd.write('        ss%' + name + ' = ival\n')
            f_uprd.write('        icount = icount + 1\n')
            f_uprd.write('     ENDIF\n')

            f_uprd.write('     jval = ss%' + name + '\n')
            f_uprd.write('     CALL ichk_dim(ival,jval,"' + name + '")\n')
            f_uprd.write('  ENDIF\n')

    f_uprd.write('\n')
    f_uprd.write('  IF(iertot.GT.0) THEN\n')
    f_uprd.write('     WRITE(iout,*) " ?ps_update_file_read: error with dimension values."\n')
    f_uprd.write('     ierr=9999\n')
    f_uprd.write('     status = nf90_close(icdf)\n')
    f_uprd.write('     RETURN\n')
    f_uprd.write('  ENDIF\n')

    f_uprd.write('\n')
    f_uprd.write('  ! DIMENSIONS read in OK; allocate state arrays...\n')
    f_uprd.write('\n')
    f_uprd.write('  IF(icount.GT.0) THEN\n')
    f_uprd.write('     CALL plasma_state_empty_alloc(ss,ierr)\n')
    f_uprd.write('     IF(ierr.NE.0) THEN\n')
    f_uprd.write('        WRITE(iout,*) " ?ps_update_file_read: plasma_state_empty_alloc error."\n')
    f_uprd.write('        ierr=9999\n')
    f_uprd.write('        status = nf90_close(icdf)\n')
    f_uprd.write('        RETURN\n')
    f_uprd.write('     ENDIF\n')
    f_uprd.write('  ENDIF\n')

    #-----------------
    f_uprd.write('\n')
    f_uprd.write('  !---------------------------------------------\n')
    f_uprd.write('  ! call subroutines to read data and arrays...\n')
    f_uprd.write('\n')

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        ff = f_uprda[isub_count]
        sname = "ps_update_file_read_" + repr(isub_count)

        arglist = "(ss,icdf,iout,iucount,iertot)\n"
        outcall = "  call " + sname + arglist
        f_uprd.write(outcall)

        sdecl = "\nSUBROUTINE " + sname + arglist
        ff.write(sdecl)
        ff.write("""

  ! Read updated data

  USE plasma_state_definition_mod
  USE plasma_hash_code
  USE netcdf
  IMPLICIT NONE

  TYPE (plasma_state) :: ss  ! state containing array elements to be allocated
  INTEGER, INTENT(IN) :: icdf  ! NetCDF file ID
  INTEGER, INTENT(IN) :: iout  ! LUN for messages
  INTEGER, INTENT(INOUT) :: iucount ! update count
  INTEGER, INTENT(INOUT) :: iertot  ! error count

  !-------------------

  INTEGER :: varid,status
  INTEGER :: ival,jval,ii
  INTEGER :: prehash1,prehash2
  INTEGER :: posthash1,posthash2
  CHARACTER*80 :: nc_errmsg
  LOGICAL :: izero

  !-------------------

""")

        isub_count = isub_count + 1


    iilist = list(all_items.keys())
    iilist.sort()

    # list of (nsplit_sce) sublists
    isublists = sublist_gen(iilist)

    isub_count = 0
    while ( isub_count < nsplit_sce ):
        sname = "ps_update_file_read_" + repr(isub_count)
        ff = f_uprda[isub_count]

        for item in isublists[isub_count]:
            if item not in all_dims:
                ff.write('\n')
                descr = all_items[item]
                kcat = elem_category(descr)

                name = descr['name']
                dtype = descr['dtype']

                dimpart = 0
                if item in grid_items:
                    dimpart = 1
                elif item in species_list_items:
                    dimpart = 1
                elif item in item_list_items:
                    dimpart = 1
                elif item in all_dims:
                    dimpart = 1

                ff.write('  status = nf90_inq_varid(icdf,"' + name +
                         '",varid)\n')
                ff.write('  IF(status.EQ.NF90_NOERR) THEN\n')
                if dimpart == 1:
                    if dtype == "I":
                        ff.write('     CALL ichk_zero(ss%' + name + ')\n')
                    elif dtype == "R":
                        ff.write('     CALL rchk_zero(ss%' + name + ')\n')
                    else:
                        ff.write('     CALL cchk_zero(ss%' + name + ')\n')

                ff.write('     status = nf90_get_var(icdf,varid,ss%' +
                         name + ')\n')
                ff.write('     IF(status.NE.NF90_NOERR) THEN\n')
                ff.write('        WRITE(iout,*) " ?ps_update_file_read: error reading variable: ' +
                         name + '"\n')
                ff.write('        CALL ncerr(status)\n')
                ff.write('        iertot = iertot + 1\n')
                ff.write('        izero=.TRUE.\n')
                ff.write('     ELSE\n')
                ff.write('        iucount = iucount + 1\n')
                ff.write('     ENDIF\n')

                if dimpart == 1:
                    ff.write('     IF(.NOT.izero) THEN\n')
                    if dtype == "I":
                        ff.write('        CALL ichk_hash("' + name +
                                 '",ss%' + name + ')\n')
                    elif dtype == "R":
                        ff.write('        CALL rchk_hash("' + name +
                                 '",ss%' + name + ')\n')
                    else:
                        ff.write('        CALL cchk_hash("' + name +
                                 '",ss%' + name + ')\n')
                    ff.write('     ENDIF\n')

                ff.write('  ENDIF\n')

        uprd_trailer(ff,sname)
        isub_count = isub_count + 1

    f_uprd.write("""

  IF(iertot.GT.0) THEN
     WRITE(iout,*) ' ?ps_update_file_read: an error occurred.'
     ierr=9999
  ELSE
     WRITE(iout,*) ' *ps_update_file_read: OK, found data for', iucount, &
          ' state elements.'
     ierr=0
  ENDIF

""")
    uprd_trailer(f_uprd,"ps_update_file_read")

def uprd_trailer(ff,sname):

    ff.write("""
CONTAINS

  subroutine ichk_zero(iarray)
     integer, dimension(:), intent(in) :: iarray

     integer :: ii

     izero=.TRUE.
     do ii=1,size(iarray)
        if(iarray(ii).ne.0) then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(iarray,prehash1,prehash2)
     endif

  end subroutine ichk_zero

  subroutine rchk_zero(rarray)
     real(kind=rspec), dimension(:), intent(in) :: rarray

     integer :: ii

     izero=.TRUE.
     do ii=1,size(rarray)
        if(rarray(ii).ne.0.0_rspec) then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(rarray,prehash1,prehash2)
     endif

  end subroutine rchk_zero

  subroutine cchk_zero(c32array)
     character*32, dimension(:), intent(in) :: c32array

     integer :: ii

     izero=.TRUE.
     do ii=1,size(c32array)
        if(c32array(ii).ne." ") then
           izero=.FALSE.
           exit
        endif
     enddo

     if(.not.izero) then
        prehash1=17
        prehash2=23
        call hashCode(c32array,prehash1,prehash2)
     endif

  end subroutine cchk_zero

  subroutine ichk_hash(itemName,iarray)
     character*(*), intent(in) :: itemName
     integer, dimension(:), intent(in) :: iarray

     posthash1=17
     posthash2=23
     call hashCode(iarray,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine ichk_hash

  subroutine rchk_hash(itemName,rarray)
     character*(*), intent(in) :: itemName
     real(kind=rspec), dimension(:), intent(in) :: rarray

     posthash1=17
     posthash2=23
     call hashCode(rarray,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine rchk_hash

  subroutine cchk_hash(itemName,c32array)
     character*(*), intent(in) :: itemName
     character*32, dimension(:), intent(in) :: c32array

     posthash1=17
     posthash2=23
     call hashCode(c32array,posthash1,posthash2)

     if((prehash1.ne.posthash1).OR.(prehash2.ne.posthash2)) then
        write(iout,*) ' ?ps_update_file_read: value change detected in dimension quantity: '//trim(itemName)
        iertot = iertot + 1
     endif

  end subroutine cchk_hash

  SUBROUTINE ichk_dim(ival,jval,dname)
     INTEGER, intent(in) :: ival  ! file dimension value
     INTEGER, intent(in) :: jval  ! state dimension value
     CHARACTER*(*), intent(in) :: dname

     IF(ival.ne.jval) THEN
        WRITE(iout,*) ' ?ps_update_file_read: update file dimension not consistent with state:'
        WRITE(iout,*) '  file value:  ',ival
        WRITE(iout,*) '  state value: ',jval
        WRITE(iout,*) '  dimension name: '//trim(dname)
        iertot = iertot + 1
     ENDIF
  END SUBROUTINE ichk_dim

  SUBROUTINE ncerr(status)
     INTEGER, INTENT(in) :: status

     nc_errmsg = NF90_strerror(status)
     write(iout,*) '  NetCDF message: ',trim(nc_errmsg)
  END SUBROUTINE ncerr

  SUBROUTINE denull(str)
     ! convert null characters to blank
     CHARACTER*(*) :: str
     INTEGER :: i

     do i=1,len(str)
        if(ichar(str(i:i)).eq.0) str(i:i)=' '
     enddo
  END SUBROUTINE denull

""")
    ff.write("END SUBROUTINE " + sname + "\n")

def plasma_state_write_xload():

    print('...plasma_state_write_xload()...')

    #-----------------------------------------------
    # GENERATE CODE

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! check for valid sets of names, non-coordinate dimensions:\n')
    f_xload.write('  ierr = 0\n')

    for dimkey in all_dims:
        ddescr = all_items[dimkey]
        dimtype = ddescr['dimtype']
        if dimtype == "S" or dimtype == "L":
            name = ddescr['name']
            name_var = ddescr['name_var']
            dlower = ddescr['dim_lval']
            dupper = name
            f_xload.write('\n')
            f_xload.write('  ahash(1) = ps_dhash_' + dimkey + '\n')
            f_xload.write('  IF(ALLOCATED(ss%' + name_var + ').AND.hcheck(1,1)) THEN\n')
            f_xload.write('     CALL ps_name_check("' + name_var + '",' +
                          dlower + ',ss%' + dupper + ',ss%' + name_var +
                          ',iout,iertmp)\n')
            f_xload.write('     ierr = max(ierr,iertmp)\n')
            f_xload.write('  ENDIF\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! generated integer enumeration labels\n')

    f_xload.write('  isize = 1\n')

    for ekey in list(all_enum_dims.keys()):
        ref = all_enum_dims[ekey]
        if ref == '(index)':
            rdescr = all_items[ekey]
            if 'dlist' in rdescr:
                evar = "max_" + ekey.lower()
            else:
                evar = ekey.lower()
            f_xload.write('  isize = max(isize,ss%' + evar + ')\n')

    f_xload.write('\n')
    f_xload.write('  ALLOCATE(enum_lbl_1d(isize))\n')
    f_xload.write('  DO i1 = 1,isize\n')
    f_xload.write('     enum_lbl_1d(i1) = " "\n')
    f_xload.write('     WRITE(enum_lbl_1d(i1),"(I8)") i1\n')
    f_xload.write('     CALL lefpack(enum_lbl_1d(i1))\n')
    f_xload.write('  ENDDO\n')

    f_xload.write('\n')
    for ekey in list(all_enum_dims.keys()):
        ref = all_enum_dims[ekey]
        if ref != '(index)':
            rdescr = all_items[ref]
            rdims = rdescr['dlist']

            rname = ref.lower()
            f_xload.write('  IF(ALLOCATED(ss%' + rname + ')) THEN\n')
            if len(rdims) == 1:
                rdim = rdims[0][1].lower()
                f_xload.write('     ALLOCATE(enum_lbl_' + rname +
                              '(ss%' + rdim + '))\n')
                f_xload.write('     enum_lbl_' + rname + ' = " "\n')
                f_xload.write('     CALL load_lbl_1d(ss%' + rname +
                              ',enum_lbl_' + rname + ')\n')
            else:
                rdim0 = rdims[0][1].lower()
                rdim_enum = rdim0[4:]  # "max_<...>" -> "<...>"
                rdim1 = rdims[1][1].lower()
                f_xload.write('     ALLOCATE(enum_lbl_' + rname +
                              '(ss%' + rdim0 + ',ss%' + rdim1 + '))\n')
                f_xload.write('     enum_lbl_' + rname + ' = " "\n')
                f_xload.write('     CALL load_lbl_2d(ss%' + rdim_enum +
                              ',ss%' + rname +
                              ',enum_lbl_' + rname + ')\n')
            f_xload.write('  ENDIF\n')

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')
    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store and perform error check on coordinate grids\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_grids(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_grids.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_grids(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:
  INTEGER :: isize,icomponent,iertmp
  CHARACTER*80 :: plabel
  INTEGER :: ahash(ps_max_static)
  !-----------------------
"""%version_id)

    for dimkey in all_dims:
        ddescr = all_items[dimkey]
        dimtype = ddescr['dimtype']
        if dimtype in all_coords:
            component = ddescr['component']
            name = ddescr['name']

            gridname = ddescr['gridname']
            gkey = gridname.upper()
            gdescr = all_items[gkey]
            cmts = gdescr['cmts']
            cmt = cmts[0]
            dcmt = cmt_dcmt(gkey,cmt)

            ccw_arg = "0"
            if 'orientation' in ddescr:
                ccwval = ddescr['orientation']
                if ccwval == "CCW":
                    ccw_arg = "ps_ccw_select"
                else:
                    ccw_arg = "ps_ccw_reverse"

            gg.write('\n')
            gg.write('  ahash(1) = ps_dhash_' + dimkey + '\n')
            gg.write('  isize = ss%' + name + '\n')
            gg.write('  IF((isize.GT.0).AND.hcheck(1,1)) THEN\n')
            gg.write('     icomponent = ps_cnum_' + component + '\n')
            gg.write('     plabel = ' + dcmt + '\n')
            gg.write('     CALL ps_grid_store(ss%iobj,"' +
                          dimtype + '","' + gridname.upper() + '",icomponent,plabel, &\n')
            gg.write('          isize,' + ccw_arg +
                          ',ss%' + gridname + ', iout,iertmp)\n')
            gg.write('     ierr=MAX(ierr,iertmp)\n')
            gg.write('  ENDIF\n')

    ps_write_xload_trailer(gg,2,"plasma_state_xload_grids")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store species lists\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_slists(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_slists.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_slists(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:
  INTEGER :: icomponent,imod
  CHARACTER*32 :: listname
  INTEGER :: ahash(ps_max_static)
  !-----------------------
"""%version_id)

    for dimkey in all_dims:
        ddescr = all_items[dimkey]
        dimtype = ddescr['dimtype']
        if dimtype == "S":
            vlist = ddescr['snames']
            rootname = ddescr['rootname'].upper()
            component = ddescr['component']

            sname = all_items[vlist[0].upper()]['name']
            stype = all_items[vlist[1].upper()]['name']
            qvar  = all_items[vlist[2].upper()]['name']
            qatom = all_items[vlist[3].upper()]['name']
            mvar  = all_items[vlist[4].upper()]['name']

            vkey = stype.upper()
            vdescr = all_items[vkey]
            dlist = vdescr['dlist']
            dlower = dlist[0][0]
            dupper = dlist[0][1]

            gg.write('\n')
            gg.write('  ahash(1) = ps_dhash_' + dimkey + '\n')
            gg.write('  IF(ALLOCATED(ss%' + sname + ').AND.hcheck(1,1)) THEN\n')
            gg.write('     listname = "PS_SPECIES_' + rootname + '"\n')
            gg.write('     icomponent = ps_cnum_' + component + '\n')
            gg.write('     CALL ps_species_list_store(ss%iobj,listname,icomponent,'
                          + dlower + ',ss%' + dupper + ', &\n')
            gg.write('          ss%' + sname + ',ss%' + qatom
                          + ',ss%' + qvar + ',ss%' + mvar
                          + ',ss%' + stype + ', &\n')
            gg.write('          imod,iout,ierr)\n')
            gg.write('     IF(ierr.NE.0) RETURN\n')
            gg.write('  ENDIF\n')

    ps_write_xload_trailer(gg,2,"plasma_state_xload_slists")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store the time (t1)\n')
    f_xload.write('\n')
    f_xload.write('  call ps_time_set(ss%iobj,ss%t1,iout,ierr)\n')
    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')
    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store scalar groups as lists\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_sgrps(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_sgrps.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_sgrps(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  INTEGER :: icomponent,imod,nitems
  CHARACTER*32 :: listname,units
  CHARACTER*80 :: label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)
  !-----------------------
"""%version_id)

    glist = list(all_groups.keys())
    glist.sort()

    for gkey in glist:
        gdata = all_groups[gkey]
        component = gdata[0]
        member1 = gdata[1]
        mdescr = all_items[member1]
        if 'dlist' not in mdescr:
            dtype = mdescr['dtype']
            if dtype == "R":
                units = mdescr['units']
            elif dtype == "I":
                units = ' '
            else:
                units = ' '
            label = 'group: ' + gkey + '; component: ' + component
            section = mdescr['section']

            gg.write('\n')
            gg.write('  ahash(1) = ps_shash_' + section + '\n')
            gg.write('  IF(hcheck(1,1)) THEN\n')
            gg.write('     listname = "' + gkey + '"\n')
            gg.write('     icomponent = ps_cnum_' + component + '\n')
            gg.write('     units = "' + units + '"\n')
            gg.write('     label = "' + label + '"\n')
            gg.write('     nitems = ' + repr(len(gdata) - 1) + '\n')

            icount = 0
            for item in gdata[1:]:
                idescr=all_items[item]
                name = idescr['name']

                if dtype == "R":
                    r8val = 'ss%' + name
                    ival = '0'
                    cval = '" "'
                elif dtype == "I":
                    r8val = '0.0_rspec'
                    ival = 'ss%' + name
                    cval = '" "'
                else:
                    r8val = '0.0_rspec'
                    ival = '0'
                    cval = 'ss%' + name
                icount = icount + 1
                gg.write('     item_names(' + repr(icount) + ') = "' + name + '"\n')
                gg.write('     item_r8vals(' + repr(icount) + ') = ' + r8val + '\n')
                gg.write('     item_ivals(' + repr(icount) + ') = ' + ival + '\n')
                gg.write('     item_c256(' + repr(icount) + ') = ' + cval + '\n')

            gg.write('\n')
            gg.write('     CALL ps_list_store("' + section + '", ss%iobj, &\n')
            gg.write('          listname,icomponent,units,label, &\n')
            gg.write('          nitems,item_names,item_ivals,item_r8vals, &\n')
            gg.write('          item_c256,imod,iout,ierr)\n')
            gg.write('     IF(ierr.NE.0) RETURN\n')
            gg.write('  ENDIF\n')


    ps_write_xload_trailer(gg,2,"plasma_state_xload_sgrps")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store standalone scalars as single valued lists\n')
    f_xload.write('  ! store 1d enumerated arrays: list + blackbox\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_singl(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_singl.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_singl(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_local = 4

  INTEGER :: icomponent,imod,nitems
  CHARACTER*32 :: listname,enum_name,units
  CHARACTER*80 :: label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_local)
  INTEGER :: item_ivals(max_local)
  REAL(KIND=rspec) :: item_r8vals(max_local)
  CHARACTER*256 :: item_c256(max_local)
  !-----------------------
"""%version_id)

    # make list that is sorted except VOL and AREA come first

    jlist = list(all_items.keys())
    jlist.sort()

    ilist = []
    ilist.append("VOL")
    ilist.append("AREA")

    ierva = 0
    for ielemi in ilist:
        descr = all_items[ielemi]
        dlist = descr['dlist']
        if 'group' not in descr and len(dlist) == 1:
            print("..." + ielemi + " OK")
        else:
            print(" ?" + ielemi + "must be a 1d profile, not in a group.")
            ierva = ierva + 1

    if ierva > 0:
        raise Vol_or_Area_error

    for ielemj in jlist:
        if ielemj != "VOL" and ielemj != "AREA":
            ilist.append(ielemj)

    loc_enum_list = []

    for ikey in ilist:
        idescr = all_items[ikey]
        kcat = elem_category(idescr)
        if 'group' in idescr:

            kcat = "skip"
        if kcat == "scalar" or kcat == "enum" or kcat == "e_dim":
            if kcat == "enum":
                enum_dim = get_enum_dim(idescr,"")
                loc_enum_list.append(ikey)
            section = idescr['section']
            component = idescr['component']
            cmts = idescr['cmts']
            label = cmt_dcmt(ikey,cmts[0])
            name = idescr['name']
            dtype = idescr['dtype']
            if dtype == "R":
                units = idescr['units']
                if kcat == "enum":
                    r8val = '0.0_rspec'
                    cval = '"__enumeration__"'
                    ival = 'ss%' + enum_dim
                else:
                    r8val = 'ss%' + name
                    cval = '" "'
                    ival = '0'
            elif dtype == "I":
                units = ' '
                r8val = '0.0_rspec'
                if kcat == "enum":
                    cval = '"__inumeration__"'
                    ival = 'ss%' + enum_dim
                else:
                    cval = '" "'
                    ival = 'ss%' + name
            else:
                units = ' '
                r8val = '0.0_rspec'
                ival = '0'
                cval = 'ss%' + name

            gg.write('\n')
            if kcat == "e_dim":
                gg.write('  ahash(1) = ps_dhash_' + name + '\n')
            else:
                gg.write('  ahash(1) = ps_shash_' + name + '\n')
            gg.write('  IF(hcheck(1,1)) THEN\n')
            gg.write('     listname = "' + ikey + '"\n')
            if kcat == "enum":
                if dtype == "I":
                    gg.write('     enum_name = "' + ikey + '__IVEC"\n')
                    subr = 'ps_inum_1d_store'
                else:
                    gg.write('     enum_name = "' + ikey + '__RVEC"\n')
                    subr = 'ps_enum_1d_store'
            gg.write('     icomponent = ps_cnum_' + component + '\n')
            gg.write('     units = "' + units + '"\n')
            gg.write('     label = ' + label + '\n')
            gg.write('     nitems = 1\n')

            gg.write('     item_names(1) = "' + name + '"\n')
            gg.write('     item_r8vals(1) = ' + r8val + '\n')
            gg.write('     item_ivals(1) = ' + ival + '\n')
            gg.write('     item_c256(1) = ' + cval + '\n')

            gg.write('     CALL ps_list_store("' + section + '", ss%iobj, &\n')
            gg.write('          listname,icomponent,units,label, &\n')
            gg.write('          nitems,item_names,item_ivals,item_r8vals, &\n')
            gg.write('          item_c256,imod,iout,ierr)\n')
            gg.write('     IF(ierr.NE.0) RETURN\n')
            if kcat == "enum":
                gg.write('     IF(ALLOCATED(ss%' + name + ')) THEN\n')
                gg.write('        CALL ' + subr +
                              '(ss%iobj,enum_name,icomponent, &\n')
                gg.write('             ' + ival + ',ss%' + name +
                              ',imod,iout,ierr)\n')
                gg.write('        IF(ierr.NE.0) RETURN\n')
                gg.write('     ENDIF\n')
            gg.write('  ENDIF\n')


    ps_write_xload_trailer(gg,2,"plasma_state_xload_singl")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store allocatable scalar arrays (non-profile data)\n')
    f_xload.write('  ! this includes 2d enumerations.\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_sarrays(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_sarrays.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_sarrays(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  INTEGER :: icomponent,imod,nitems,isize
  CHARACTER*32 :: listname,units,enum_name
  CHARACTER*80 :: label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  INTEGER :: i1,i2,i3,i4
  CHARACTER*32 :: nam1,nam2,nam3,nam4
  CHARACTER*80 :: ilab1,ilab2,ilab3,ilab4

  !-----------------------
"""%version_id)

    for ikey in ilist:
        if ikey not in all_profiles and ikey not in species_list_items and ikey not in grid_items and ikey not in loc_enum_list:
            idescr = all_items[ikey]
            kcat = elem_category(idescr)
            if kcat == "array":
                [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",idescr,"standard")
            elif kcat == "enum_array":
                [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",idescr,"enum")
                enum_dim = get_enum_dim(idescr,"")
            if kcat == "array" or kcat == "enum_array":
                name = idescr['name']
                spec = idescr['spec']
                component = idescr['component']
                section = idescr['section']
                cmts = idescr['cmts']
                label = cmt_dcmt(ikey,cmts[0])

                dtype = idescr['dtype']
                if dtype == "R":
                    units = idescr['units']
                    if kcat == "enum_array":
                        r8val = '0.0_rspec'
                        cval = '"__enumeration__ "//ilab1'
                        # drop "max_" prefix
                        ival = 'ss%' + enum_dim[4:] + dstr
                    else:
                        r8val = 'ss%' + name + dstr
                        ival = '0'
                        cval = 'ILAB1'
                elif dtype == "I":
                    units = ' '
                    r8val = '0.0_rspec'
                    if kcat == "enum_array":
                        cval = '"__inumeration__ "//ilab1'
                        # drop "max_" prefix
                        ival = 'ss%' + enum_dim[4:] + dstr
                    else:
                        ival = 'ss%' + name + dstr
                        cval = 'ILAB1'
                else:
                    units = ' '
                    r8val = '0.0_rspec'
                    ival = '0'
                    cval = 'ss%' + name + dstr

                if ikey in item_list_items:
                    dlist = idescr['dlist']
                    dkey = dlist[0][1].upper()
                    aahash = 'ps_dhash_' + dkey
                elif 'dimtype' in idescr:
                    aahash = 'ps_dhash_' + ikey
                else:
                    aahash = 'ps_shash_' + name

                gg.write('\n')
                gg.write('  ahash(1) = ' + aahash + '\n')
                gg.write('  !------------------------\n')
                gg.write('  IF(ALLOCATED(ss%' + name + ').AND.hcheck(1,1)) THEN\n')

                gg.write('     ! ' + spec + '\n')
                gg.write('     listname = "' + ikey + '"\n')
                if kcat == "enum_array":
                    if dtype == "I":
                        gg.write('     enum_name = "' + ikey + '__IVEC"\n')
                        subr = 'ps_inum_2d_store'
                    else:
                        gg.write('     enum_name = "' + ikey + '__RVEC"\n')
                        subr = 'ps_enum_2d_store'
                gg.write('     icomponent = ps_cnum_' + component + '\n')
                gg.write('     units = "' + units + '"\n')
                gg.write('     label = ' + label + '\n')
                gg.write('     nitems = 0\n')
                indent = "     "  # indentation of first DO loop
                indent = start_loop_i1234nam(gg,indent,"ss%",idloop,namvars,namlbls,idescr)
                indent = indent + "   "
                gg.write(indent + 'nitems = nitems + 1\n')

                gg.write(indent + 'IF(nitems.GT.max_itemlist) THEN\n')
                indent = indent + "   "
                gg.write(indent + 'WRITE(iout,*) " ?plasma_state_xload:' +
                              '  max list size (",max_itemlist,") exceeded."\n')
                gg.write(indent + 'WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"\n')
                gg.write(indent + 'ierr = 9999\n')
                gg.write(indent + 'RETURN\n')
                indent = indent[:-3]
                gg.write(indent + 'ENDIF\n')

                gg.write(indent + 'item_names(nitems) = TRIM(listname)' +
                              '//"_"//TRIM(NAM1)\n')
                gg.write(indent + 'item_r8vals(nitems) = ' + r8val + '\n')
                gg.write(indent + 'item_ivals(nitems) = ' + ival + '\n')
                gg.write(indent + 'item_c256(nitems) = ' + cval + '\n')

                indent = indent[:-3]
                indent = end_loop_i1234(gg,indent,idloop)
                gg.write('\n')
                gg.write('     CALL ps_list_store("' + section + '",ss%iobj, &\n')
                gg.write('          listname,icomponent,units,label, &\n')
                gg.write('          nitems,item_names,item_ivals,item_r8vals, &\n')
                gg.write('          item_c256,imod,iout,ierr)\n')
                gg.write('     IF(ierr.NE.0) RETURN\n')
                if kcat == "enum_array":
                    gg.write('\n')
                    gg.write('     isize = size(ss%' + name + ',1)\n')
                    gg.write('     CALL ' + subr +
                                  '(ss%iobj,enum_name,icomponent, &\n')
                    gg.write('          nitems,item_ivals(1:nitems),isize, &\n')
                    gg.write('          ss%' + name + ', imod,iout,ierr)\n')
                    gg.write('     IF(ierr.NE.0) RETURN\n')
                gg.write('  ENDIF\n')

    ps_write_xload_trailer(gg,2,"plasma_state_xload_sarrays")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store singleton profiles with single valued lists\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_psingl(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_psingl.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_psingl(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_local = 4

  INTEGER :: icomponent,imod,nitems,idchk
  CHARACTER*32 :: listname,units,list_units
  CHARACTER*80 :: label,plabel
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_local)
  INTEGER :: item_ivals(max_local)
  REAL(KIND=rspec) :: item_r8vals(max_local)
  CHARACTER*256 :: item_c256(max_local)
  !-----------------------
"""%version_id)

    # find profiles that satisfy (a) coordinate dimensions are the only
    # dimensions and (b) do not belong to a group, and (c) is not a derivative
    # of another profile

    for ikey in ilist:
        if ikey in all_profiles and ikey not in deriv_list:
            descr = all_items[ikey]
            dlist = descr['dlist']
            rank = descr['rank_coord']
            methkey = descr['methkey']
            if 'group' not in descr and rank == len(dlist):

                component = descr['component']
                [wtype,woper,wfun,sunits] = get_smethod(descr)
                alias = descr['alias']
                if wtype == "wght":
                    xlabel = wtype + " " + woper + " (" + wfun + "): " + alias
                else:
                    xlabel = wtype + " " + woper + ": " + alias

                [rank,grid_list,dim_list,ccwa] = get_dlist(descr)


                if methkey == "HERMITE_EXPLICIT":
                    call_root = 'CALL ps_' + repr(rank) + 'dx_prof_store(ss%iobj,'
                else:
                    call_root = 'CALL ps_' + repr(rank) + 'd_prof_store(ss%iobj,'

                grid_args = ""
                dim_args = ""

                for grid in grid_list:
                    grid_args = grid_args + '"' + grid + '",'
                grid_args = grid_args + " &"

                for dim in dim_list:
                    dim_args = dim_args + 'ss%' + dim + ','
                for ccw in ccwa:
                    dim_args = dim_args + ccw + ','
                dim_args = dim_args + " &"

                [itype,bc_opt] = interp_flags(descr)

                if methkey == "HERMITE_EXPLICIT":
                    arg2 = 'units,icomponent,plabel, &'
                else:
                    arg2 = 'units,icomponent,plabel,' + itype + ',' + bc_opt + ', &'

                dtype = descr['dtype']
                units = descr['units']
                section = descr['section']

                name1 = descr['name']

                gg.write('\n')

                icnum = gen_ahash(gg,[ikey],0)
                gg.write('  idchk = ss%id_' + name1 + '\n')
                gg.write('  IF(ALLOCATED(ss%' + name1 + ').AND.hcheck(' + repr(icnum) + ',idchk)) THEN\n')
                gg.write('     listname = "' + ikey + '_LIST"\n')
                gg.write('     icomponent = ps_cnum_' + component + '\n')
                gg.write('     units = "' + units + '"\n')
                gg.write('     nitems = 1\n')
                gg.write('     ! for assoc. scalar list:\n')
                gg.write('     list_units = "' + sunits + '"\n')
                gg.write('     label = "' + xlabel + '"\n')

                name = descr['name']
                alias = descr['alias']
                cmts = descr['cmts']
                label1 = cmt_dcmt(ikey,cmts[0])

                nam_args = 'ss%' + name + ','
                if methkey == "HERMITE_EXPLICIT":
                    loc_deriv_list = descr['loc_deriv_list']
                    for deriv_key in loc_deriv_list:
                        deriv_name = all_items[deriv_key]['name']
                        nam_args = nam_args + 'ss%' + deriv_name + ','
                nam_args = nam_args + 'ss%id_' + name + ', &'

                gg.write('\n')
                gg.write('     plabel = ' + label1 + '\n')
                str = '     ' + call_root + '"' + alias + '",' + grid_args
                gg.write(str + '\n')
                gg.write('          ' + arg2 + '\n')
                gg.write('          ' + dim_args + '\n')
                gg.write('          ' + nam_args + '\n')
                gg.write('          imod,iout,ierr)\n')
                gg.write('     IF(ierr.NE.0) RETURN\n')
                r8val = '0.0_rspec'
                ival = 'ss%id_' + name
                cval = 'plabel'
                gg.write('     item_names(1) = "' + alias + '"\n')
                gg.write('     item_r8vals(1) = ' + r8val + '\n')
                gg.write('     item_ivals(1) = ' + ival + '\n')
                gg.write('     item_c256(1) = ' + cval + '\n')

                gg.write('\n')
                gg.write('     CALL ps_list_store("STATE_PROFILES", ss%iobj, &\n')
                gg.write('          listname,icomponent,list_units,label, &\n')
                gg.write('          nitems,item_names,item_ivals,item_r8vals, &\n')
                gg.write('          item_c256,imod,iout,ierr)\n')
                gg.write('     IF(ierr.NE.0) RETURN\n')
                gg.write('  ENDIF\n')

    ps_write_xload_trailer(gg,2,"plasma_state_xload_psingl")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store profiles in groups; groups themselves as lists\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_pgrps(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_pgrps.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_pgrps(ss,xhash,iout,ierr)

  ! called from plasma_state_xload: check grids; store in xplasma structure

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  INTEGER :: icomponent,imod,nitems,idchk
  CHARACTER*32 :: listname,units,list_units
  CHARACTER*80 :: label,plabel
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  !-----------------------
"""%version_id)

    for gkey in glist:
        gdata = all_groups[gkey]
        component = gdata[0]
        member1 = gdata[1]
        mdescr = all_items[member1]
        if 'dlist' in mdescr:
            [wtype,woper,wfun,sunits] = get_smethod(mdescr)
            alias = mdescr['alias']
            if wtype == "wght":
                xlabel = wtype + " " + woper + " (" + wfun + "): " + alias
            else:
                xlabel = wtype + " " + woper + ": " + alias

            [rank,grid_list,dim_list,ccwa] = get_dlist(mdescr)

            methkey = mdescr['methkey']
            if methkey == "HERMITE_EXPLICIT":
                call_root = 'CALL ps_' + repr(rank) + 'dx_prof_store(ss%iobj,'
            else:
                call_root = 'CALL ps_' + repr(rank) + 'd_prof_store(ss%iobj,'

            grid_args = ""
            dim_args = ""

            for grid in grid_list:
                grid_args = grid_args + '"' + grid + '",'
            grid_args = grid_args + " &"

            for dim in dim_list:
                dim_args = dim_args + 'ss%' + dim + ','
            for ccw in ccwa:
                dim_args = dim_args + ccw + ','
            dim_args = dim_args + " &"

            [itype,bc_opt] = interp_flags(mdescr)

            if methkey == "HERMITE_EXPLICIT":
                arg2 = 'units,icomponent,plabel, &'
            else:
                arg2 = 'units,icomponent,plabel,' + itype + ',' + bc_opt + ', &'

            dtype = mdescr['dtype']
            units = mdescr['units']
            label = 'group: ' + gkey + '; component: ' + component
            section = mdescr['section']

            name1 = mdescr['name']

            gg.write('\n')
            icnum = gen_ahash(gg,gdata[1:],1)
            gg.write('  IF(ALLOCATED(ss%' + name1 + ').AND.hcheck(' + repr(icnum) + ',idchk)) THEN\n')
            gg.write('     listname = "' + gkey + '"\n')
            gg.write('     icomponent = ps_cnum_' + component + '\n')
            gg.write('     units = "' + units + '"\n')
            gg.write('     nitems = ' + repr(len(gdata) - 1) + '\n')
            gg.write('     ! for assoc. scalar list:\n')
            gg.write('     list_units = "' + sunits + '"\n')
            gg.write('     label = "' + xlabel + '"\n')

            icount = 0
            for item in gdata[1:]:
                idescr=all_items[item]
                name = idescr['name']
                alias = idescr['alias']
                cmts = idescr['cmts']
                label1 = cmt_dcmt(item,cmts[0])

                nam_args = 'ss%' + name + ','
                if methkey == "HERMITE_EXPLICIT":
                    loc_deriv_list = idescr['loc_deriv_list']
                    for deriv_key in loc_deriv_list:
                        deriv_name = all_items[deriv_key]['name']
                        nam_args = nam_args + 'ss%' + deriv_name + ','
                nam_args = nam_args + 'ss%id_' + name + ', &'

                gg.write('\n')
                gg.write('     plabel = ' + label1 + '\n')
                str = '     ' + call_root + '"' + alias + '",' + grid_args
                gg.write(str + '\n')
                gg.write('          ' + arg2 + '\n')
                gg.write('          ' + dim_args + '\n')
                gg.write('          ' + nam_args + '\n')
                gg.write('          imod,iout,ierr)\n')
                gg.write('     IF(ierr.NE.0) RETURN\n')
                r8val = '0.0_rspec'
                ival = 'ss%id_' + name
                cval = 'plabel'
                icount = icount + 1
                gg.write('     item_names(' + repr(icount) + ') = "' + alias + '"\n')
                gg.write('     item_r8vals(' + repr(icount) + ') = ' + r8val + '\n')
                gg.write('     item_ivals(' + repr(icount) + ') = ' + ival + '\n')
                gg.write('     item_c256(' + repr(icount) + ') = ' + cval + '\n')

            gg.write('\n')
            gg.write('     CALL ps_list_store("STATE_PROFILES", ss%iobj, &\n')
            gg.write('          listname,icomponent,list_units,label, &\n')
            gg.write('          nitems,item_names,item_ivals,item_r8vals, &\n')
            gg.write('          item_c256,imod,iout,ierr)\n')
            gg.write('     IF(ierr.NE.0) RETURN\n')
            gg.write('  ENDIF\n')


    ps_write_xload_trailer(gg,2,"plasma_state_xload_pgrps")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! store indexed arrays of profiles\n')
    f_xload.write('  ! "parrays" subroutine handles non-enumerated subset.\n')
    f_xload.write('\n')
    f_xload.write('  CALL plasma_state_xload_parrays(ss,xhash,iout,ierr)\n')

    gg = open("plasma_state_xload_parrays.f90","w")

    gg.write("""
! written by plasma_state_gen.py
!   input: plasma_state_spec.dat, version id: %s
! *** do not edit *** (use code generator instead)

SUBROUTINE plasma_state_xload_parrays(ss,xhash,iout,ierr)

  USE plasma_state_definition_mod
  IMPLICIT NONE

  TYPE (plasma_state) :: ss     ! state data to write into xplasma
  INTEGER :: xhash(ps_hash_total)  ! hash data
  INTEGER, INTENT(IN) :: iout   ! LUN for messages
  INTEGER, INTENT(INOUT) :: ierr   ! completion code (set if error occurs)

  !-----------------------
  ! local:

  INTEGER, PARAMETER :: max_itemlist = ps_max_static

  INTEGER :: icomponent,imod,jmod,nitems,idchk
  CHARACTER*32 :: listname,units,list_units,profname,iiencod
  CHARACTER*80 :: label,plabel,list_label
  INTEGER :: ahash(ps_max_static)

  CHARACTER*32 :: item_names(max_itemlist)
  INTEGER :: item_ivals(max_itemlist)
  REAL(KIND=rspec) :: item_r8vals(max_itemlist)
  CHARACTER*256 :: item_c256(max_itemlist)

  INTEGER :: i1,i2,i3,i4,isize
  CHARACTER*32 :: nam1,nam2,nam3,nam4
  CHARACTER*80 :: ilab1,ilab2,ilab3,ilab4

  !-----------------------
  ! for enumeration labeling...
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: enum_lbl_1d
"""%version_id)

    for enum in list(all_enum_dims.keys()):
        ref = all_enum_dims[enum]
        if ref != "(index)":
            rdescr = all_items[ref]
            rdims = rdescr['dlist']
            if len(rdims) == 1:
                gg.write('  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: enum_lbl_' + ref + '\n')
            else:
                gg.write('  CHARACTER*32, DIMENSION(:,:), ALLOCATABLE :: enum_lbl_' + ref + '\n')

    gg.write("""   """)

    gg.write('\n')
    gg.write('  !----------------------------------------------------\n')
    gg.write('  ! generated integer enumeration labels\n')

    gg.write('  isize = 1\n')

    for ekey in list(all_enum_dims.keys()):
        ref = all_enum_dims[ekey]
        if ref == '(index)':
            rdescr = all_items[ekey]
            if 'dlist' in rdescr:
                evar = "max_" + ekey.lower()
            else:
                evar = ekey.lower()
            gg.write('  isize = max(isize,ss%' + evar + ')\n')

    gg.write('\n')
    gg.write('  ALLOCATE(enum_lbl_1d(isize))\n')
    gg.write('  DO i1 = 1,isize\n')
    gg.write('     enum_lbl_1d(i1) = " "\n')
    gg.write('     WRITE(enum_lbl_1d(i1),"(I8)") i1\n')
    gg.write('     CALL lefpack(enum_lbl_1d(i1))\n')
    gg.write('  ENDDO\n')

    gg.write('\n')
    for ekey in list(all_enum_dims.keys()):
        ref = all_enum_dims[ekey]
        if ref != '(index)':
            rdescr = all_items[ref]
            rdims = rdescr['dlist']

            rname = ref.lower()
            gg.write('  IF(ALLOCATED(ss%' + rname + ')) THEN\n')
            if len(rdims) == 1:
                rdim = rdims[0][1].lower()
                gg.write('     ALLOCATE(enum_lbl_' + rname +
                              '(ss%' + rdim + '))\n')
                gg.write('     enum_lbl_' + rname + ' = " "\n')
                gg.write('     CALL load_lbl_1d(ss%' + rname +
                              ',enum_lbl_' + rname + ')\n')
            else:
                rdim0 = rdims[0][1].lower()
                rdim_enum = rdim0[4:]  # "max_<...>" -> "<...>"
                rdim1 = rdims[1][1].lower()
                gg.write('     ALLOCATE(enum_lbl_' + rname +
                              '(ss%' + rdim0 + ',ss%' + rdim1 + '))\n')
                gg.write('     enum_lbl_' + rname + ' = " "\n')
                gg.write('     CALL load_lbl_2d(ss%' + rdim_enum +
                              ',ss%' + rname +
                              ',enum_lbl_' + rname + ')\n')
            gg.write('  ENDIF\n')

    gg.write('\n')

    # this loop is for profile arrays that are also have non-coordinate
    # indices e.g. plasma species lists

    for ikey in ilist:
        if ikey in all_profiles and ikey not in deriv_list:
            idescr = all_items[ikey]
            rank = idescr['rank_coord']
            dlist = idescr['dlist']
            if rank < len(dlist):
                idrank = len(dlist) - rank
                methkey = idescr['methkey']
                component = idescr['component']

                [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",idescr,"standard")
                if enum_maxdim == "TRUE":
                    hh = f_xload
                else:
                    hh = gg

                # generated combined dimension string
                icr = 0
                full_dstr = ""
                while icr < rank:
                    if icr == 0:
                        full_dstr = full_dstr + "(:"
                    else:
                        full_dstr = full_dstr + ",:"
                    icr = icr + 1
                full_dstr = full_dstr + "," + dstr[1:]

                # typical dstr values: "(I1)", "(I1,I2)"
                # typical full_dstr value: "(:,I1)", "(:,:,I1,I2)"

                # generate 1st id dimension (1) or (1,1) or (1,1,1):
                icidx = 0
                id111 = ""
                while icidx < idrank:
                    if icidx == 0:
                        id111 = id111 + "(1"
                    else:
                        id111 = id111 + ",1"
                    icidx = icidx + 1
                id111 = id111 + ")"

                [wtype,woper,wfun,sunits] = get_smethod(idescr)
                alias = idescr['alias']
                if wtype == "wght":
                    xlabel = wtype + " " + woper + " (" + wfun + "): " + alias
                else:
                    xlabel = wtype + " " + woper + ": " + alias

                if xlabel[-1:] == "_":
                    # trim trailing underscore, if present
                    xlabel = xlabel[:-1]

                [rank,grid_list,dim_list,ccwa] = get_dlist(idescr)

                if methkey == "HERMITE_EXPLICIT":
                    call_root = 'CALL ps_' + repr(rank) + 'dx_prof_store(ss%iobj,'
                else:
                    call_root = 'CALL ps_' + repr(rank) + 'd_prof_store(ss%iobj,'

                grid_args = ""
                dim_args = ""

                for grid in grid_list:
                    grid_args = grid_args + '"' + grid + '",'
                grid_args = grid_args + " &"

                for dim in dim_list:
                    dim_args = dim_args + 'ss%' + dim + ','
                for ccw in ccwa:
                    dim_args = dim_args + ccw + ','
                dim_args = dim_args + " &"

                [itype,bc_opt] = interp_flags(idescr)

                if methkey == "HERMITE_EXPLICIT":
                    arg2 = 'units,icomponent,plabel, &'
                else:
                    arg2 = 'units,icomponent,plabel,' + itype + ',' + bc_opt + ', &'

                name = idescr['name']
                nam_args = 'ss%' + name + full_dstr + ','
                if methkey == "HERMITE_EXPLICIT":
                    loc_deriv_list = idescr['loc_deriv_list']
                    for deriv_key in loc_deriv_list:
                        deriv_name = all_items[deriv_key]['name']
                        nam_args = nam_args + 'ss%' + deriv_name + full_dstr + ','
                nam_args = nam_args + 'ss%id_' + name + dstr + ', &'

                dtype = idescr['dtype']
                units = idescr['units']
                section = idescr['section']

                cmts = idescr['cmts']
                cmt = cmts[0]
                dcmt = cmt_dcmt(ikey,cmt)

                hh.write('\n')

                icnum = gen_ahash(hh,[ikey],0)
                hh.write('  idchk = 0\n')
                hh.write('  IF(ALLOCATED(ss%id_' + name +
                              ')) idchk = ss%id_' + name + id111 + '\n')
                hh.write('  IF(ALLOCATED(ss%' + name + ').AND.hcheck(' + repr(icnum) + ',idchk)) THEN\n')
                hh.write('     listname = "' + ikey + '_LIST"\n')
                hh.write('     icomponent = ps_cnum_' + component + '\n')
                hh.write('     units = "' + units + '"\n')
                hh.write('     label = ' + dcmt + '\n')
                hh.write('     ! for assoc. scalar list:\n')
                hh.write('     list_units = "' + sunits + '"\n')
                hh.write('     list_label = "' + xlabel + '"\n')
                hh.write('\n')
                hh.write('     nitems = 0\n')
                hh.write('     jmod = 0\n')

                indent = "     "  # indentation of first DO loop
                indent = start_loop_i1234nam(hh,indent,"ss%",idloop,namvars,namlbls,idescr)
                indent = indent + "   "
                hh.write(indent + 'nitems = nitems + 1\n')

                hh.write(indent + 'IF(nitems.GT.max_itemlist) THEN\n')
                indent = indent + "   "
                hh.write(indent + 'WRITE(iout,*) " ?plasma_state_xload:' +
                              '  max list size (",max_itemlist,") exceeded."\n')
                hh.write(indent + 'WRITE(iout,*) "  regen plasma state software with larger value of ps_max_static in plasma_state_spec.dat"\n')
                hh.write(indent + 'ierr = 9999\n')
                hh.write(indent + 'RETURN\n')
                indent = indent[:-3]
                hh.write(indent + 'ENDIF\n')

                hh.write(indent + 'item_names(nitems) = "' + alias +
                              '"//TRIM(NAM1)\n')
                hh.write(indent + 'item_c256(nitems) = ILAB1\n')
                hh.write(indent + 'plabel = TRIM(label)//": "//TRIM(nam1)\n')
                hh.write(indent + 'profname = "' + alias + '"//TRIM(nam1)\n')
                str = indent + call_root + 'profname,' + grid_args
                hh.write(str + '\n')
                hh.write(indent + '     ' + arg2 + '\n')
                hh.write(indent + '     ' + dim_args + '\n')
                hh.write(indent + '     ' + nam_args + '\n')
                hh.write(indent + '     imod,iout,ierr)\n')
                hh.write(indent + 'IF(ierr.NE.0) RETURN\n')
                hh.write(indent + 'IF(imod.gt.0) jmod = 1\n')
                r8val = '0.0_rspec'
                ival = 'ss%id_' + name + dstr
                hh.write(indent + 'item_r8vals(nitems) = ' + r8val + '\n')
                hh.write(indent + 'item_ivals(nitems) = ' + ival + '\n')
                indent = indent[:-3]
                indent = end_loop_i1234(hh,indent,idloop)
                hh.write('\n')
                hh.write('     CALL ps_list_store("STATE_PROFILES", ss%iobj, &\n')
                hh.write('          listname,icomponent,list_units,list_label, &\n')
                hh.write('          nitems,item_names,item_ivals,item_r8vals, &\n')
                hh.write('          item_c256,imod,iout,ierr)\n')
                hh.write('     IF(ierr.NE.0) RETURN\n')
                hh.write('  ENDIF\n')


    ps_write_xload_trailer(gg,1,"plasma_state_xload_parrays")
    gg.close()

    f_xload.write('\n')
    f_xload.write('  IF(ierr.GT.0) RETURN\n')

    f_xload.write("""
  !----------------------------------------------------
  ! ALL PROFILES STORED -- update weight IDs

  CALL plasma_state_zonwt_update(ss,ierr)
  IF(ierr.NE.0) return

  !  check if rezone integrals are available (this depends on an equilibrium
  !  being included in the state).

  CALL ps_rezone_chk(ss,id_intg,iout,ierr)
  IF(ierr.NE.0) return

  !  id_intg=0 indicates no rezone available.
""")

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! profile group list values updated here...\n')

    for gkey in glist:
        gdata = all_groups[gkey]
        component = gdata[0]
        member1 = gdata[1]
        mdescr = all_items[member1]
        if 'dlist' in mdescr:
            name1 = mdescr['name']
            [wtype,woper,wfun,sunits] = get_smethod(mdescr)

            # one test for whole group
            f_xload.write('\n')
            icnum = gen_ahash(f_xload,gdata[1:],0)
            if wtype == "edge" or wtype == "simple":
                f_xload.write('  IF(ALLOCATED(ss%' + name1 + ').AND.hcheck(' + repr(icnum) + ',1)) THEN\n')
            else:
                f_xload.write('  IF(ALLOCATED(ss%' + name1 + ').AND.(id_intg.GT.0).AND.hcheck(' + repr(icnum) + ',1)) THEN\n')

            f_xload.write('     listname = "' + gkey + '"\n')
            f_xload.write('     icomponent = ps_cnum_' + component + '\n')
            f_xload.write('     nitems = ' + repr(len(gdata[1:])) + '\n')

            icg = 0
            for member in gdata[1:]:
                icg = icg + 1
                mdescr = all_items[member]
                name = mdescr['name']
                if wtype == "simple":
                    expr = 'sum(ss%' + name + ')/size(ss%' + name + ')'
                    f_xload.write('     ansvec(1,1) = ' + expr + '\n')

                elif wtype == "edge":
                    dlist = mdescr['dlist']
                    dupper = dlist[0][1]
                    expr = 'ss%' + name + '(ss%' + dupper + ')'
                    f_xload.write('     ansvec(1,1) = ' + expr + '\n')

                else:
                    if wtype == "area":
                        curdens = ".TRUE."
                    else:
                        curdens = ".FALSE."
                    if woper == "int":
                        nonorm = ".TRUE."
                    else:
                        nonorm = ".FALSE."

                    f_xload.write('     idvec(1) = ss%id_' + name + '\n')
                    f_xload.write('     idwvec(1) = ss%id_zonwt(idvec(1))\n')
                    str1 = '     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &'
                    str2 = '          ' + curdens + ',' + nonorm
                    str2 = str2 + ',.FALSE.,ansvec,dvvec,iout,ierr)'
                    f_xload.write(str1 + '\n')
                    f_xload.write(str2 + '\n')
                    f_xload.write('     IF(ierr.NE.0) RETURN\n')
                f_xload.write('     item_r8vals(' + repr(icg) + ') = ansvec(1,1)\n')
            f_xload.write('\n')
            f_xload.write('     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &\n')
            f_xload.write('          nitems,item_r8vals,iout,ierr)\n')
            f_xload.write('     IF(ierr.NE.0) RETURN\n')
            f_xload.write('  ENDIF\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! list values for singleton profiles\n')

    # selection logic same as singleton profiles loop, above.

    for ikey in ilist:
        if ikey in all_profiles and ikey not in deriv_list:
            descr = all_items[ikey]
            dlist = descr['dlist']
            rank = descr['rank_coord']
            methkey = descr['methkey']
            if 'group' not in descr and rank == len(dlist):

                [wtype,woper,wfun,sunits] = get_smethod(descr)
                name = descr['name']
                component = descr['component']
                f_xload.write('\n')
                icnum = gen_ahash(f_xload,[ikey],0)
                if wtype == "edge" or wtype == "simple":
                    f_xload.write('  IF(ALLOCATED(ss%' + name + ').AND.hcheck(' + repr(icnum) + ',1)) THEN\n')
                else:
                    f_xload.write('  IF(ALLOCATED(ss%' + name + ').AND.(id_intg.GT.0).AND.hcheck(' + repr(icnum) + ',1)) THEN\n')

                f_xload.write('     listname = "' + ikey + '_LIST"\n')
                f_xload.write('     icomponent = ps_cnum_' + component + '\n')
                f_xload.write('     nitems = 1\n')

                if wtype == "simple":
                    expr = 'sum(ss%' + name + ')/size(ss%' + name + ')'
                    f_xload.write('     ansvec(1,1) = ' + expr + '\n')

                elif wtype == "edge":
                    dupper = dlist[0][1]
                    expr = 'ss%' + name + '(ss%' + dupper + ')'
                    f_xload.write('     ansvec(1,1) = ' + expr + '\n')

                else:
                    if wtype == "area":
                        curdens = ".TRUE."
                    else:
                        curdens = ".FALSE."
                    if woper == "int":
                        nonorm = ".TRUE."
                    else:
                        nonorm = ".FALSE."

                    f_xload.write('     idvec(1) = ss%id_' + name + '\n')
                    f_xload.write('     idwvec(1) = ss%id_zonwt(idvec(1))\n')
                    str1 = '     call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &'
                    str2 = '          ' + curdens + ',' + nonorm
                    str2 = str2 + ',.FALSE.,ansvec,dvvec,iout,ierr)'
                    f_xload.write(str1 + '\n')
                    f_xload.write(str2 + '\n')
                    f_xload.write('     IF(ierr.NE.0) RETURN\n')
                f_xload.write('     item_r8vals(1) = ansvec(1,1)\n')
                f_xload.write('\n')
                f_xload.write('     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &\n')
                f_xload.write('          nitems,item_r8vals,iout,ierr)\n')
                f_xload.write('     IF(ierr.NE.0) RETURN\n')
                f_xload.write('  ENDIF\n')

    f_xload.write('\n')
    f_xload.write('  !----------------------------------------------------\n')
    f_xload.write('  ! update list data on indexed arrays of profiles\n')

    # this loop is for profile arrays that are also have non-coordinate
    # indices e.g. plasma species lists

    for ikey in ilist:
        if ikey in all_profiles and ikey not in deriv_list:
            idescr = all_items[ikey]
            rank = idescr['rank_coord']
            dlist = idescr['dlist']
            if rank < len(dlist):
                name = idescr['name']
                component = idescr['component']

                [idloop,dstr,namvars,namlbls,enum_maxdim] = gen_idloop("ss%",idescr,"standard")

                [wtype,woper,wfun,sunits] = get_smethod(idescr)

                f_xload.write('\n')
                icnum = gen_ahash(f_xload,[ikey],0)
                if wtype == "edge" or wtype == "simple":
                    f_xload.write('  IF(ALLOCATED(ss%' + name + ').AND.hcheck(' + repr(icnum) + ',1)) THEN\n')
                else:
                    f_xload.write('  IF(ALLOCATED(ss%' + name + ').AND.(id_intg.GT.0).AND.hcheck(' + repr(icnum) + ',1)) THEN\n')

                f_xload.write('     listname = "' + ikey + '_LIST"\n')
                f_xload.write('     icomponent = ps_cnum_' + component + '\n')
                f_xload.write('     nitems = 0\n')

                indent = "     "  # indentation of first DO loop
                indent = start_loop_i1234(f_xload,indent,idloop)
                indent = indent + "   "

                if enum_maxdim != "TRUE":
                    f_xload.write(indent + 'nitems = nitems + 1\n')
                if wtype == "simple":
                    irank = 0
                    dstra = dstr[:1]
                    while irank < rank:
                        irank = irank + 1
                        dstra = dstra + ":,"
                    dstra = dstra + dstr[1:]
                    expr = 'sum(ss%' + name + dstra + ')/size(ss%' + name + dstra + ')'
                    f_xload.write(indent + 'ansvec(1,1) = ' + expr + '\n')

                elif wtype == "edge":
                    dupper = dlist[0][1]
                    expr = 'ss%' + name + '(ss%' + dupper + ')'
                    f_xload.write(indent + 'ansvec(1,1) = ' + expr + '\n')

                else:
                    if wtype == "area":
                        curdens = ".TRUE."
                    else:
                        curdens = ".FALSE."
                    if woper == "int":
                        nonorm = ".TRUE."
                    else:
                        nonorm = ".FALSE."

                    f_xload.write(indent + 'idvec(1) = ss%id_' + name + dstr + '\n')
                    if enum_maxdim=="TRUE":
                        f_xload.write(indent + 'IF(idvec(1).EQ.0) CYCLE\n')
                        f_xload.write(indent + 'nitems = nitems + 1\n')
                    f_xload.write(indent + 'idwvec(1) = ss%id_zonwt(idvec(1))\n')
                    str1 = indent + 'call ps_rezone_rho(ss,2,x01,1,idvec,idwvec, &'
                    str2 = indent + '     ' + curdens + ',' + nonorm
                    str2 = str2 + ',.FALSE.,ansvec,dvvec,iout,ierr)'
                    f_xload.write(str1 + '\n')
                    f_xload.write(str2 + '\n')
                    f_xload.write(indent + 'IF(ierr.NE.0) RETURN\n')

                f_xload.write(indent + 'item_r8vals(nitems) = ansvec(1,1)\n')
                indent = indent[:-3]
                indent = end_loop_i1234(f_xload,indent,idloop)
                f_xload.write('\n')
                f_xload.write('     CALL ps_list_update_r8(ss%iobj,listname,icomponent, &\n')
                f_xload.write('          nitems,item_r8vals,iout,ierr)\n')
                f_xload.write('     IF(ierr.NE.0) RETURN\n')
                f_xload.write('  ENDIF\n')

    f_xload.write('\n')
    for ekey in list(all_enum_dims.keys()):
        ref = all_enum_dims[ekey]
        if ref != '(index)':
            rname = ref.lower()
            f_xload.write('  IF(ALLOCATED(ss%' + rname + ')) THEN\n')
            f_xload.write('     DEALLOCATE(enum_lbl_' + rname + ')\n')
            f_xload.write('  ENDIF\n')

    f_xload.write("""
  ! OK ALL DONE: update hash codes

  ss%hash_codes = ss%new_hash_codes
  xhash = ss%new_hash_codes

  call ps_xhashes_put(ss%iobj,xhash,iout,ierr)

  deallocate(enum_lbl_1d)
""")

    ps_write_xload_trailer(f_xload,1,"plasma_state_xload")

def ps_write_xload_trailer(ff,iopt,sname):

    ff.write("""
CONTAINS

  LOGICAL FUNCTION hcheck(inum,idchk)
     INTEGER, intent(in) :: inum
     INTEGER, intent(in) :: idchk

     !  check all hash codes: set function value TRUE if any show changes.
     !  also set TRUE if idchk = 0

     INTEGER :: ia1,ia2,ii

     if(idchk.eq.0) then
        hcheck=.TRUE.
        return
     endif

     hcheck = .FALSE.
     DO ii=1,inum
        hcheck = .TRUE.
        ia1 = ahash(ii)
        ia2 = ia1 + 1
        IF(ss%hash_codes(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(ss%hash_codes(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        IF(xhash(ia1).NE.ss%new_hash_codes(ia1)) EXIT
        IF(xhash(ia2).NE.ss%new_hash_codes(ia2)) EXIT
        hcheck = .FALSE.
     ENDDO
  END FUNCTION hcheck
""")

    if iopt == 1:
        ff.write("""
  SUBROUTINE load_lbl_1d(indx_1d,elabel)
     integer, dimension(:), intent(in) :: indx_1d
     character*32, dimension(:), intent(inout) :: elabel

     integer :: iwarn,idup,ii,jj

     iwarn=0
     do ii=1,size(indx_1d)
        if(indx_1d(ii).lt.0) then
           write(elabel(ii),'("m",i8)') -indx_1d(ii)
        else
           write(elabel(ii),'("p",i8)') indx_1d(ii)
        endif

        idup=0
        do jj=1,ii-1
           if(indx_1d(ii).eq.indx_1d(jj)) idup=idup+1
        enddo

        if(idup.gt.0) then
           if(iwarn.eq.0) then
              iwarn=1
              write(iout,*) ' %plasma_state_xload: enumeration index values not unique (patch applied).'
           endif
           write(elabel(ii)(10:18),'("x",i8)') idup+1
        endif

        call lefpack(elabel(ii))
     enddo
  END SUBROUTINE load_lbl_1d

  SUBROUTINE load_lbl_2d(indx_num,indx_2d,elabel)
     integer, dimension(:), intent(in) :: indx_num
     integer, dimension(:,:), intent(in) :: indx_2d
     character*32, dimension(:,:), intent(inout) :: elabel

     integer :: kk,inum

     do kk=1,size(indx_num)
        inum = indx_num(kk)
        if(inum.gt.0) then
           call load_lbl_1d(indx_2d(1:inum,kk),elabel(1:inum,kk))
        endif
     enddo
  END SUBROUTINE load_lbl_2d
""")

    ff.write("""
  SUBROUTINE lefpack(str)
     CHARACTER*(*), intent(inout) :: str
     !  left pack non-blank characters in str

     INTEGER :: ii,jj
     ii=0
     DO jj=1,len(str)
        IF(str(jj:jj).NE.' ') THEN
           ii = ii + 1
           str(ii:ii) = str(jj:jj)
        ENDIF
     ENDDO
     str(ii+1:) = ' '
  END SUBROUTINE lefpack
""")
    ff.write("\nEND SUBROUTINE " + sname + "\n")

def plasma_state_write_pspcat():

    f_pspcat.write("""
SUBROUTINE ps_print_category(ilun,iname)

  ! print category to which named item (iname) belongs
  ! any item which contains (iname) as a substring is printed

  implicit NONE

  integer, intent(in) :: ilun
  character*(*), intent(in) :: iname

  !------------------------------------
  ! local:

  character*32 :: uname  ! iname -> uppercase
  character*32 :: pname  ! iname in form to print
  integer :: ii,ict

  logical, save :: sym_init = .FALSE.
  character*32, dimension(:,:), allocatable, save :: sym_list

  !------------------------------------
""")

    alist = list(all_items.keys())
    alist.sort()

    inum = len(alist)

    f_pspcat.write("  integer, parameter :: numsym = " + repr(inum) + "\n")

    f_pspcat.write("""
  !------------------------------------

  if(.NOT.sym_init) then
     sym_init = .TRUE.
     allocate(sym_list(2,numsym))
""")

    isub    = 0   # index of int subroutine being filled
    islines = 0   # number of lines in init subroutine
    smore   = []  # lines of additional init subroutines to add to file after ps_print_category()

    ict = 0
    for item in alist:
        ict = ict + 1

        cat = "???"
        if item in grid_items:
            cat = "grid"
        elif item in item_list_items:
            cat = "item_list"
        elif item in species_list_items:
            cat = "species_list"
        elif item in deriv_list:
            cat = "derivative_profile"
        elif item in enum_maxlist:
            cat = "enumeration_limit"
        elif item in mdescr:
            cat = "machine_description"
        elif item in sconfig:
            cat = "shot_configuration"
        elif item in simcon:
            cat = "simulation_initialization"
        elif item in state_data_list:
            cat = "state_data"
        elif item in profile_list:
            cat = "profile"

        line = "   sym_list(1,%d) = '%s'; sym_list(2,%d) = '%s'\n"%(ict,item,ict,cat)

        if (islines==0):      # start a new init subroutine
            if (isub>0):
                smore.append("END SUBROUTINE ps_print_category_init%d\n\n"%isub)

            isub += 1
            f_pspcat.write("     call ps_print_category_init%d(2, numsym, sym_list)\n"%isub)

            smore.append("SUBROUTINE ps_print_category_init%d(i1,i2,sym_list)\n"%isub)
            smore.append("   implicit none\n")
            smore.append("   integer,      intent(in)    :: i1,i2\n")
            smore.append("   character*32, intent(inout) :: sym_list(i1,i2)\n\n")

        smore.append(line)
        islines = (islines+1)%100   # limit number of init lines per init subroutine

    if (isub>0):
        smore.append("END SUBROUTINE ps_print_category_init%d\n"%isub)

    f_pspcat.write("  endif\n")

    f_pspcat.write("""
  uname = iname
  call uupper(uname)

  ict = 0
  do ii=1,numsym
     if(index(sym_list(1,ii),trim(uname)).gt.0) then
        ict = ict + 1
        pname = adjustr(sym_list(1,ii))
        write(ilun,'(A)') "  "//pname//":  "//trim(sym_list(2,ii))
     endif
  enddo

  if(ict.eq.0) then
     write(ilun,*) ' *** nothing found for: '//trim(uname)
  endif

END SUBROUTINE ps_print_category

""")
    for x in smore:
        f_pspcat.write(x)     # add in init subroutines

def plasma_state_write_namio():

    print('...plasma_state_write_namio()...')

    #-----------------------------------------------
    # GENERATE CODE to write namelist files

    write_namwr(f_wmd,mdescr_namel)

    f_wmd.write("""

  WRITE(ilun_namel,'(" /")')
  CLOSE(UNIT=ilun_namel)

END SUBROUTINE ps_mdescr_namelist_write
""")

    write_namwr(f_wsc,sconfig_namel)

    f_wsc.write("""

  WRITE(ilun_namel,'(" /")')
  CLOSE(UNIT=ilun_namel)

END SUBROUTINE ps_sconfig_namelist_write
""")

    #-----------------------------------------------
    # GENERATE CODE to read namelist files

    write_namrd(f_rmd,mdescr_namel,"mdescr")

    f_rmd.write("""

  IF(g_filename.NE.' ') THEN
     CALL ps_mdescr_gfile_read(g_filename,ss,ierr)
     IF(ierr.NE.0) THEN
        WRITE(iout,*) ' ?ps_mdescr_namelist_read: gfile read error.'
     ENDIF
  ENDIF

END SUBROUTINE ps_mdescr_namelist_read
""")

    write_namrd(f_rsc,sconfig_namel,"sconfig")

    f_rsc.write("""

END SUBROUTINE ps_sconfig_namelist_read
""")


def write_namwr(f,namel):

    # generate code to write commented namelist
    # code calls to hand written module routines

    for item in namel:

        vartype = item[0]
        varname = item[1]
        vardim  = item[2]

        array = "FALSE"
        enum_array = "FALSE"
        vardim_list = vardim.split(",")
        if len(vardim_list) > 1:
            enum_array = "TRUE"
            vardim1 = vardim_list[0]
            vardim2 = vardim_list[1]
            if vardim1[-2:] == "()":
                vardim1 = vardim1[:-2]

        elif vardim != "":
            array = "TRUE"

        if vartype == "S":
            key = varname.upper() + '_NAME'
        else:
            key = varname.upper()

        descr = all_items[key]
        cmts = descr['cmts']
        cmt = cmts[0]
        dcmt = cmt_dcmt(key,cmt)

        if vartype == "I":
            units = "integer"

        elif vartype == "R":
            units = descr['units']

        elif vartype == "S":
            units = "species_list"

        else:
            units = "character"

        dunits = cmt_dcmt(key,units)

        if enum_array == "FALSE" and array == "FALSE":
            f.write('\n')
            f.write('  CALL ps_namwr(ilun_namel, &\n')
            f.write('       ' + dcmt + ',' + dunits + ', &\n')
        elif array == "TRUE":
            f.write('\n')
            if vartype == "S":
                f.write('  IF(ALLOCATED(ss%' + varname  + '_name)) THEN\n')
            else:
                f.write('  IF(ALLOCATED(ss%' + varname  + ')) THEN\n')
            f.write('     CALL ps_namwr(ilun_namel, &\n')
            f.write('          ' + dcmt + ',' + dunits + ', &\n')
        elif enum_array == "TRUE":
            f.write('\n')
            f.write('  IF(ALLOCATED(ss%' + varname  + ')) THEN\n')
            f.write('     ALLOCATE(isize_vec(ss%' + vardim2 + '))\n')
            f.write('     isize_vec = ss%' + vardim1 + '\n')
            f.write('     CALL ps_namwr(ilun_namel, &\n')
            f.write('          ' + dcmt + ',' + dunits + ', &\n')

        if len(vardim) == 0:
            # scalar

            f.write('       "' + varname + '", ss%' + varname + ')\n')

        elif vartype == "S":
            # species list
            # skip electrons

            dimps = vardim.split(':')
            if len(dimps) == 1:
                varlen = vardim
            else:
                varlen = dimps[1]

            dimstr = "1:ss%" + varlen

            f.write('          "' + varname + '", ss%' + varlen +
                    ', ss%' + varname + '_name(' + dimstr + '), &\n')
            f.write('          ss%qatom_' + varname + '(' + dimstr
                    + '), ss%q_' + varname + '(' + dimstr
                    + '), ss%m_' + varname + '(' + dimstr + '))\n')
            f.write('  ELSE\n')
            f.write('     CALL ps_namwr(ilun_namel, &\n')
            f.write('          ' + dcmt + ',' + dunits +
                    ', "' + varname + '")\n')
            f.write('  ENDIF\n')

        else:
            # array; just pass all of it, with length argument...

            if enum_array == "FALSE":
                f.write('          "' + varname + '", ss%' + vardim +
                        ', ss%' + varname + ')\n')
                f.write('  ELSE\n')
                f.write('     CALL ps_namwr(ilun_namel, &\n')
                f.write('          ' + dcmt + ',' + dunits +
                        ', "' + varname + '")\n')
                f.write('  ENDIF\n')
            else:
                f.write('          "' + varname + '", isize_vec' +
                        ', ss%' + varname + ')\n')
                f.write('     DEALLOCATE(isize_vec)\n')
                f.write('  ELSE\n')
                f.write('     CALL ps_namwr(ilun_namel, &\n')
                f.write('          ' + dcmt + ',' + dunits +
                        ', "' + varname + '")\n')
                f.write('  ENDIF\n')

def write_namrd(f,namel,namel_name):

    # generate code to read commented namelist
    # this involves:
    #   (a) declaration of a set of static variables to hold namelist contents.
    #   (b) declaration of namelist.
    #   (c) initialization of static variables from existing state elements.
    #   (d) call to subroutine to open namelist & temporary files; write
    #       temporary file containing namelist only with comments stripped.
    #   (e) read the namelist.
    #   (f) allocate additional array elements in state as needed.
    #   (g) transfer data from static variables to state elements.
    #   (h) close files and exit

    ilen = len(namel)

    if namel_name == "mdescr":
        f_mdmod.write("""

MODULE mdescr_data

  ! Plasma State machine description data -- standalone module

  implicit NONE
  SAVE
  PUBLIC

  INTEGER, PARAMETER :: rspec = SELECTED_REAL_KIND(p=15,r=307)

""")

        klist = all_constants["PS_MAX_STATIC"]
        ps_max_static = klist[1]

        klist = all_constants["PS_MAX_STATIC_2D"]
        ps_max_static_2d = klist[1]

        print(" --> ps_max_static = " + ps_max_static)
        print(" --> ps_max_static_2d = " + ps_max_static_2d)
        f_mdmod.write("  INTEGER, PARAMETER :: ps_max_static = " +
                      ps_max_static + "\n")
        f_mdmod.write("  INTEGER, PARAMETER :: ps_max_static_2d = " +
                      ps_max_static_2d + "\n")

        f_mdmod.write("\n")
        f_mdmod.write("  !---------------------------------\n")
        f_mdmod.write("  INTEGER :: iout = 6\n")
        f_mdmod.write("  !---------------------------------\n")
        f_mdmod.write("\n")

        f_mdinit.write("""

SUBROUTINE mdescr_init

  ! initialize mdescr_data

  USE mdescr_data
  implicit NONE

  !-------------------------------

""")

        f_mdread.write("""

SUBROUTINE mdescr_read(filename,ierr)

  ! read "mdescr" namelist -- generate a temporary filename

  character*(*), intent(in) :: filename
  integer, intent(out) :: ierr

  character*120 tmp_filename
  integer :: ilen

  call tmpfile_d('mdescr',tmp_filename,ilen)

  call mdescr_read_tmpnam(filename,tmp_filename,ierr)

END SUBROUTINE mdescr_read

SUBROUTINE mdescr_read_tmpnam(filename,tmp_filename,ierr)

  ! read "mdescr" namelist -- temporary filename provided for copy
  ! with comments stripped out

  USE mdescr_data
  implicit NONE

  character*(*), intent(in) :: filename
  character*(*), intent(in) :: tmp_filename
  integer, intent(out) :: ierr

  !-------------------------------
  integer :: ilun_namel,iertmp
  !-------------------------------

  ierr = 0

  ! OK: open namelist file, strip comments.
  !     namelist to be read on ilun_namel after this call:

  CALL ps_namel_strip(filename,tmp_filename,ilun_namel,iout,ierr)

  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?namelist file OPEN or READ error: '//trim(filename)
     RETURN
  ENDIF

  READ(UNIT=ilun_namel,NML=mdescr)

  CLOSE(UNIT=ilun_namel,status="DELETE")

""")

    locdim_2d = []
    locdim_vec = []

    # look for dimensions of 2d arrays & enum vector dimensions
    for item in namel:
        vardim = item[2].strip()
        if vardim != "":
            vardim_list = vardim.split(",")
            if len(vardim_list) == 2:
                for dim in vardim_list:
                    if dim[-2:] == "()":
                        dimnam = dim[:-2]
                        if dimnam not in locdim_2d:
                            locdim_2d.append(dimnam)
                        if dimnam not in locdim_vec:
                            locdim_vec.append(dimnam)
                    else:
                        if dim not in locdim_2d:
                            locdim_2d.append(dim)

    locdim = []
    locdima = []
    locvar_dim2d = []
    for item in namel:

        vartype = item[0]
        varname = item[1]
        vardim  = item[2]

        vardim_list = []
        if len(vardim) > 0:
            tmplist = vardim.split(",")
            for dimv in tmplist:
                dim=dimv.strip()
                if dim[-2:] == "()":
                    vardim_list.append(dim[:-2])
                else:
                    vardim_list.append(dim)

        if len(vardim_list) == 2:
            static_dim = 'ps_max_static_2d,ps_max_static_2d'
            locvar_dim2d.append(varname)
        elif len(vardim_list) == 1:
            dim=vardim_list[0]
            if dim in locdim_2d:
                static_dim = 'ps_max_static_2d'
                locvar_dim2d.append(varname)
            else:
                static_dim = 'ps_max_static'

        if vartype == "I":
            decl = "INTEGER"
            if len(vardim_list) == 0:
                locdim.append(varname.upper())
            else:
                if len(vardim_list) == 1:
                    locdima.append(varname.upper())

        elif vartype == "R":
            decl = "REAL(KIND=rspec)"
        elif vartype == "S":
            decl = "INTEGER"
        elif vartype == "N":
            decl = "CHARACTER*32"
        elif vartype == "F":
            decl = "CHARACTER*256"
        elif vartype == "L":
            decl = "CHARACTER*32"
        elif vartype[0:2] == "C*":
            decl = "CHARACTER" + vartype[1:]
        else:
            print(" ?write_namrd: unrecognized variable type: " + vartype)
            print("  namelist variable name: " + varname)
            raise  write_namrd_error

        if len(vardim) == 0:
            str = '  ' + decl + ' :: ' + varname + '\n'
            f.write(str)
            if namel_name == "mdescr":
                f_mdmod.write(str)

        else:
            if vartype != "S":
                str = '  ' + decl + ', dimension(' + static_dim + ') :: ' + varname + '\n'
                f.write(str)
                if namel_name == "mdescr":
                    f_mdmod.write(str)
                    if vartype == "L":
                        str2 = '  INTEGER :: ' + vardim + ' = 0   ! ** dimension variable **\n\n'
                        f_mdmod.write(str2)

            else:
                f.write('  ' + decl + ', dimension(' + static_dim + ') :: iAMU_' +
                        varname + '\n')
                f.write('  ' + decl + ', dimension(' + static_dim + ') :: iZatom_' +
                        varname + '\n')
                f.write('  ' + decl + ', dimension(' + static_dim + ') :: iZion_' +
                        varname + '\n')
                if namel_name == "mdescr":
                    print(" ?namrd_write: species list not expected in mdescr namelist.")
                    raise mdescr_error


    f.write('\n')
    f.write('  NAMELIST /' + namel_name + '/ &\n')

    if namel_name == "mdescr":
        f_mdmod.write('\n')
        f_mdmod.write('  NAMELIST /' + namel_name + '/ &\n')

    icount = 0
    for item in namel:
        icount = icount + 1
        vartype = item[0]
        varname = item[1]

        if vartype == "S":
            varlis = 'iAMU_' + varname + ',iZatom_' + varname + ',iZion_' + varname
        else:
            varlis = varname

        if icount < ilen:
            f.write('       ' + varlis + ', &\n')
            if namel_name == "mdescr":
                f_mdmod.write('       ' + varlis + ', &\n')
        else:
            f.write('       ' + varlis + '\n')
            if namel_name == "mdescr":
                f_mdmod.write('       ' + varlis + '\n')
                f_mdmod.write('\n')
                f_mdmod.write('END MODULE mdescr_data\n')

    f.write('  !---------------------------------------\n')
    f.write('\n')
    f.write('  iout = SWIM_err_out_file\n')
    f.write('\n')
    f.write('  ierr = 0\n')
    f.write('\n')

    f.write('  ! initialize local namelist variables to state values\n')

    for item in namel:

        vartype = item[0]
        varname = item[1]
        vardim  = item[2]

        tmplist = vardim.split(",")

        varkey = varname.upper()

        if vartype == "S":
            f.write('  iAMU_' + varname + ' = ps_unInit\n')
            f.write('  iZatom_' + varname + ' = ps_unInit\n')
            f.write('  iZion_' + varname + ' = ps_unInit\n')

        elif vartype == "I":
            if len(vardim) == 0:
                f.write('  IF(init) ss%' + varname + ' = 0\n')
                f.write('  ' + varname + ' = ss%' + varname + '\n')
            else:
                f.write('  ' + varname + ' = ps_unInit\n')
            if namel_name == "mdescr":
                f_mdinit.write('  ' + varname + ' = 0\n')
        elif vartype == "R":
            if len(vardim) == 0:
                f.write('  IF(init) ss%' + varname + ' = 0.0_rspec\n')
                f.write('  ' + varname + ' = ss%' + varname + '\n')
            else:
                f.write('  ' + varname + ' = 0.0_rspec\n')
            if namel_name == "mdescr":
                f_mdinit.write('  ' + varname + ' = 0.0_rspec\n')
        else:
            if len(vardim) == 0:
                f.write('  IF(init) ss%' + varname + ' = " "\n')
                f.write('  ' + varname + ' = ss%' + varname + '\n')
            else:
                f.write('  ' + varname + ' = " "\n')
            if namel_name == "mdescr":
                f_mdinit.write('  ' + varname + ' = " "\n')

        if len(vardim) > 0 and vartype != "S":
            if len(tmplist) == 1:
                f.write('  IF(ALLOCATED(ss%' + varname + ')) THEN\n')
                f.write('     isize = size(' + varname + ')\n')
                f.write('     IF(init) THEN\n')
                f.write('        ss%' + varname + '(1:isize) = ' + varname +
                        '(1:isize)\n')
                f.write('     ELSE\n')
                f.write('        ' + varname + '(1:isize) = ss%' + varname +
                        '(1:isize)\n')
                f.write('     ENDIF\n')
                f.write('  ENDIF\n')
            else:
                f.write('  IF(ALLOCATED(ss%' + varname + ')) THEN\n')
                f.write('     isiz0 = size(' + varname + ',1)\n')
                f.write('     isize = size(' + varname + ',2)\n')
                f.write('     IF(init) THEN\n')
                f.write('        ss%' + varname + '(1:isiz0,1:isize) = ' +
                        varname + '(1:isiz0,1:isize)\n')
                f.write('     ELSE\n')
                f.write('        ' + varname + '(1:isiz0,1:isize) = ss%' +
                        varname + '(1:isiz0,1:isize)\n')
                f.write('     ENDIF\n')
                f.write('  ENDIF\n')


    if namel_name == "mdescr":
        f_mdinit.write("\n")
        f_mdinit.write("END SUBROUTINE mdescr_init\n")

    for item in namel:

        vartype = item[0]
        varname = item[1]
        vardim  = item[2]

        tmplist = vardim.split(",")

        vartest = ""

        if vartype == "S":
            vartest = varname + '_type'

        if len(vartest) > 0:
            f.write('\n')
            f.write('  IF(ALLOCATED(ss%' + vartest + ')) THEN\n')
            f.write('     ierr = ierr + 1\n')
            f.write('     write(iout,*) " ?' + namel_name +
                    ' namelist read error: state element ' +
                    varname.upper() + ' already allocated."\n')
            f.write('  ENDIF\n')

    f.write("""
  IF(ierr.GT.0) RETURN

  ! OK: open namelist file, strip comments.
  !     namelist to be read on ilun_namel after this call:

  CALL ps_namel_strip(filename,tmp_filename,ilun_namel,iout,ierr)

  IF(ierr.NE.0) THEN
     WRITE(iout,*) ' ?namelist file OPEN or READ error: '//trim(filename)
     RETURN
  ENDIF
""")

    f.write('\n')
    f.write('  READ(UNIT=ilun_namel,NML=' + namel_name + ')\n')
    f.write('\n')
    f.write('  CLOSE(UNIT=ilun_namel,status="DELETE")\n')
    f.write('\n')
    f.write('  ! check list quantities; infer dimensions\n')

    for item in namel:

        vartype = item[0]
        varname = item[1]
        uvar = varname.upper()
        vardim  = item[2]

        if vartype == "L":
            if varname in locvar_dim2d:
                dimstat = "ps_max_static_2d"
            else:
                dimstat = "ps_max_static"
            f.write('\n')
            f.write('  CALL ps_namrd_ilist_chk("' + varname + '", &\n')
            f.write('       ' + dimstat + ',' +
                    varname + ',ss%' + vardim + ',iout,iertmp)\n')
            f.write('  IF(iertmp.NE.0) ierr = ierr + 1\n')

            if namel_name == "mdescr":
                f_mdread.write('\n')
                f_mdread.write('  CALL ps_namrd_ilist_chk("' + varname + '", &\n')
                f_mdread.write('       ' + dimstat + ',' +
                               varname + ',' + vardim + ',iout,iertmp)\n')
                f_mdread.write('  IF(iertmp.NE.0) ierr = ierr + 1\n')


        elif vartype == "S":
            if varname in locvar_dim2d:
                dimstat = "ps_max_static_2d"
            else:
                dimstat = "ps_max_static"
            if vardim[0:2] == "0:":
                vardim2 = vardim[2:]
            else:
                vardim2 = vardim

            f.write('\n')
            f.write('  CALL ps_namrd_slist_chk("' + varname + '", &\n')
            f.write('       ' + dimstat + ',iAMU_' +
                    varname + ',iZatom_' + varname + ',iZion_' +
                    varname + ',ss%' + vardim2 + ',iout,iertmp)\n')
            f.write('  IF(iertmp.NE.0) ierr = ierr + 1\n')

        elif uvar in all_dims and uvar not in locdima:
            f.write('\n')
            f.write('  ss%' + varname + ' = ' + varname + '\n')

    f.write('\n')
    f.write('  IF(ierr.NE.0) THEN\n')
    f.write('     WRITE(iout,*) " ?' + namel_name + ' namelist list check errors after read."\n')
    f.write('     RETURN\n')
    f.write('  ENDIF\n')

    if namel_name == "mdescr":
        f_mdread.write('\n')
        f_mdread.write('  IF(ierr.NE.0) THEN\n')
        f_mdread.write('     WRITE(iout,*) " ?' + namel_name + ' namelist list check errors after read."\n')
        f_mdread.write('     RETURN\n')
        f_mdread.write('  ENDIF\n')
        f_mdread.write('\n')
        f_mdread.write('END SUBROUTINE mdescr_read_tmpnam\n')

    f.write('\n')
    f.write('  ! OK, allocate arrays per dimensioning in namelist\n')
    f.write('  CALL plasma_state_empty_alloc(ss,ierr)\n')
    f.write('  IF(ierr.NE.0) RETURN\n')
    f.write('\n')
    f.write('  ! Now pick up any vector dimensions\n')

    icount = 0
    for item in namel:

        vartype = item[0]
        varname = item[1]
        uvar = varname.upper()
        vardim  = item[2]

        if uvar in locdima:
            icount = icount + 1
            f.write('\n')
            f.write('  isize = ss%' + vardim + '\n')
            f.write('  ss%' + varname + '(1:isize) = ' +
                    varname + '(1:isize)\n')

    if icount > 0:
        f.write('\n')
        f.write('  ! OK, allocate arrays per vector dimensioning in namelist\n')
        f.write('  CALL plasma_state_empty_alloc(ss,ierr)\n')
        f.write('  IF(ierr.NE.0) RETURN\n')
        f.write('\n')

    f.write('  ! Now copy namelist variables into state\n')

    for item in namel:

        vartype = item[0]
        varname = item[1]
        uvar = varname.upper()
        vardim  = item[2]
        tmplist = vardim.split(",")
        dimlist = []
        if len(tmplist) > 1:
            for dim in tmplist:
                if dim[-2:] == "()":
                    dimlist.append(dim[:-2].upper())
                else:
                    dimlist.append(dim.upper())
        elif len(vardim) > 0:
            dimlist = tmplist

        if vartype == "S":
            f.write('\n')
            f.write('  IF(ALLOCATED(ss%qatom_' + varname + ')) THEN\n')
            if vardim[0:2] == "0:":
                f.write('     CALL ps_species_convert(-1, -1, 0, &\n')
                f.write('          ss%qatom_' + varname + '(0), ' +
                        'ss%q_' + varname + '(0), ' +
                        'ss%m_' + varname + '(0), ierr)\n')
                f.write('     IF(ierr.NE.0) RETURN\n')
                f.write('\n')
                varlim = vardim[2:]
            else:
                varlim = vardim

            f.write('     DO II = 1,ss%' + varlim + '\n')
            f.write('        CALL ps_species_convert(' +
                    'iZatom_' + varname + '(ii),' +
                    'iZion_' + varname + '(ii),' +
                    'iAMU_' + varname + '(ii), &\n')
            f.write('             ss%qatom_' + varname + '(ii), ' +
                    'ss%q_' + varname + '(ii), ' +
                    'ss%m_' + varname + '(ii), ierr)\n')
            f.write('        IF(ierr.NE.0) RETURN\n')
            f.write('     ENDDO\n')
            f.write('  ENDIF\n')
            f.write('\n')

        elif len(vardim) > 0 and uvar not in all_dims:
            f.write('\n')
            f.write('  IF(ALLOCATED(ss%' + varname + ')) THEN\n')
            if len(dimlist) == 1:
                f.write('     isize = ss%' + vardim + '\n')
                f.write('     ss%' + varname + '(1:isize) = ' +
                        varname + '(1:isize)\n')
            else:
                uchk = dimlist[0]
                if uchk in locdima:
                    f.write('     isiz0 = maxval(ss%' + uchk + ')\n')
                else:
                    f.write('     isiz0 = ss%' + uchk + '\n')
                f.write('     isize = ss%' + dimlist[1] + '\n')
                f.write('     ss%' + varname + '(1:isiz0,1:isize) = ' +
                        varname + '(1:isiz0,1:isize)\n')

            f.write('  ENDIF\n')

        elif uvar not in all_dims:
            f.write('  ss%' + varname + ' = ' + varname + '\n')

def gen_ahash(ff,glist,iarg):
    # generate list of hash code addresses to check:
    # 1 for each profile in group-list; insert derivative profiles as well.
    # return the total number.

    icnum = 0

    for item in glist:
        icnum = icnum + 1
        ff.write('  ahash(' + repr(icnum) + ') = ps_phash_' + item + '\n')
        if iarg == 1:
            if icnum == 1:
                ff.write('  idchk = ss%id_' + item + '\n')
            else:
                ff.write('  idchk = min(idchk,ss%id_' + item + ')\n')
        if 'loc_deriv_list' in all_items[item]:
            loc_deriv_list = all_items[item]['loc_deriv_list']
            for d_item in loc_deriv_list:
                icnum = icnum + 1
                ff.write('  ahash(' + repr(icnum) + ') = ps_phash_' + d_item + '\n')

    return icnum

def start_loop_i1234nam(f,indent,prefix,idloop,namvars,namlbls,idescr):

    # generate code
    #    DO I4=a,b
    #       DO I3=c,d ...
    # idloop[] contains for each level [loop_variable,lower_limit,upper_limit]
    # namvars[],namlbls[] contains labeling information for each level

    indent = indent[:-3]
    suffixi = ""
    suffixn = ""
    indx = -1
    for itriple in idloop:
        [ivar,dlower,dupper] = itriple
        inum = ivar[-1:]

        indent = indent + "   "
        f.write(indent + "DO " + ivar + " = " + dlower + "," + dupper + "\n")

        indx = indx + 1
        namvar = namvars[indx]
        namlbl = namlbls[indx]

        inden2 = indent + "   "
        if namvar[0:5] != "(enum":
            f.write(inden2 + "NAM" + inum + " = TRIM(" + prefix + namvar
                    + "(" + ivar + "))" + suffixn + "\n")
            f.write(inden2 + "ILAB" + inum + " = '" + namlbl + ": '//TRIM(" +
                    prefix + namvar + "(" + ivar + "))" + suffixi + "\n")
        elif namvar[5:6] == ')':
            f.write(inden2 + "NAM" + inum + " = TRIM(enum_lbl_1d(" +
                    ivar + "))" + suffixn + "\n")
            f.write(inden2 + "ILAB" + inum + " = '('//TRIM(enum_lbl_1d(" +
                    ivar + "))//')'" + suffixi + "\n")
        else:
            ref = namvar[6:-1]  # extract name from "(enum:<ref>)"
            rkey = ref.upper()
            rdescr = all_items[rkey]
            rdims = rdescr['dlist']
            if len(rdims) == 1:
                eref = "enum_lbl_" + ref + "(" + ivar + ")"
                f.write(inden2 + "IF(" + eref + "(1:1).eq.' ') CYCLE\n")
                f.write(inden2 + "NAM" + inum + " = TRIM(" + eref + ")" +
                        suffixn + "\n")
                f.write(inden2 + "iiencod = ' '\n")
                f.write(inden2 + "WRITE(iiencod,'(I8)') " + prefix + ref +
                        "(" + ivar + ")\n")
                f.write(inden2 + "CALL lefpack(iiencod)\n")
                f.write(inden2 + "ILAB" + inum + " = '('//TRIM(iiencod)//" +
                        "')'" + suffixi + "\n")
            else:
                icodim = idescr['enum_codim']
                icoind = "I" + repr(icodim)
                eref = "enum_lbl_" + ref + "(" + ivar + "," + icoind + ")"
                f.write(inden2 + "IF(" + eref + "(1:1).eq.' ') CYCLE\n")
                f.write(inden2 + "NAM" + inum + " = TRIM(" + eref +
                        ")" + suffixn + "\n")
                f.write(inden2 + "iiencod = ' '\n")
                f.write(inden2 + "WRITE(iiencod,'(I8)') " + prefix + ref +
                        "(" + ivar + "," + icoind + ")\n")
                f.write(inden2 + "CALL lefpack(iiencod)\n")
                f.write(inden2 + "ILAB" + inum + " = '" + ref +
                        "='//TRIM(iiencod)" + suffixi + "\n")

        suffixn = "//'_'//TRIM(NAM" + inum + ")"
        suffixi = "//' '//TRIM(ILAB" + inum + ")"
    return indent

def get_smethod(descr):

    # determine method for reducing profile to a single representative
    # number for scalar list.  Possibilities are:
    #   (default): simple average sum(profile)/(#pts in profile)
    #   area int
    #   vol int
    #   vol avg
    #   wght avg
    #   edge value

    # rules:
    #   any profiles not of form  f(rho)  get the default
    #
    #   for profiles of form f(rho):
    #     if in component EQ and units = "Pa": vol avg
    #                        rank > 1: vol avg
    #                        units <> "Pa": edge value
    #
    #     if not in component EQ:
    #         step*dA -> area int
    #         step*dV -> vol int
    #         step*{expr} -> wght avg
    #         units are: "m^-3" -> vol int; output units = "#"
    #         units are: "m^-3/sec" -> vol int; output units = "#/sec"
    #
    #     if none of the above:  use the default

    units = descr['units']
    rank_coord = descr['rank_coord']

    wtype = "simple"
    woper = "avg"
    wfun = " "
    sunits = units

    if rank_coord > 1:
        return [wtype,woper,wfun,sunits]

    dlist = descr['dlist']

    #  this is e.g. "nrho" or "nrho-1":
    gdim = dlist[0][1]
    if gdim[-2:] == "-1":
        #  get the root name
        gdim = gdim[:-2]

    gkey = gdim.upper()
    coord = all_items[gkey]['dimtype']

    if coord != "RHO":
        return [wtype,woper,wfun,sunits]

    component = descr['component']

    if component == "EQ":
        irank = len(descr['dlist'])
        if units == "Pa" or irank > 1:
            wtype = "vol"
        else:
            wtype = "edge"
            woper = "value"
        return [wtype,woper,wfun,sunits]

    methkey = descr['methkey']
    factor  = descr['factor']

    if factor != " ":
        if methkey == "STEP":
            if factor == "PS_DA":
                wtype = "area"
                woper = "int"
            elif factor == "PS_DV":
                wtype = "vol"
                woper = "int"
            else:
                wtype = "wght"
                woper = "avg"
                wlist = factor.split("(")
                wfun = wlist[0]
                if wfun in ["NS","NMINI","NBEAMI","NFUSI"]:
                    wfun = "density"
    else:
        if units == "m^-3":
            wtype = "vol"
            woper = "int"
            sunits = "#"
        elif units == "m^-3/sec":
            wtype = "vol"
            woper = "int"
            sunits = "#/sec"

    return [wtype,woper,wfun,sunits]

def get_dlist(descr):

    # return list containing [rank,[coordinate-list],[dimension-info],[ccwa]]
    # e.g.: [2,["RHO","TH"],["nrho_eq","nth_eq"],["0","ps_ccw_select"]]
    # the 2nd list contains the actual array dimensions; e.g. "-1" is possible
    # the 3rd list contains information on orientation of angle coordinates

    rank = descr['rank_coord']
    dlist = descr['dlist']

    grid_list = []
    dim_list = []
    ccwa = []

    idim = 0
    while idim < rank:
        dupper = dlist[idim][1]
        idim = idim + 1

        dim_list.append(dupper)

        gdim = dupper
        if gdim[-2:] == "-1":
            gdim = gdim[:-2]
        gkey = gdim.upper()
        gridname = all_items[gkey]['gridname']
        grid_list.append(gridname.upper())

        if 'orientation' in all_items[gkey]:
            orientation = all_items[gkey]['orientation']
            if orientation == "CCW":
                ccwa.append("ps_ccw_select")
            else:
                ccwa.append("ps_ccw_reverse")
        else:
            ccwa.append("0")

    return [rank,grid_list,dim_list,ccwa]

def interp_flags(descr):

    # return interpolation controls based on interpolation/storage method

    methkey = descr['methkey']
    factor = descr['factor']

    if methkey == "STEP":
        itype = 'ps_stepfun'
        if factor == "PS_DV":
            itype = 'ps_stepdv'
        elif factor == "PS_DA":
            itype = 'ps_stepda'
        bc_opt= 'ps_bc_dflt'
    elif methkey == "PCLIN":
        itype = 'ps_pclin'
        bc_opt= 'ps_bc_dflt'
    elif methkey == "HERMITE":
        itype = 'ps_hermite'
        bc_opt= 'ps_bc_dflt'
    elif methkey == "HERMITE_00":
        itype = 'ps_hermite'
        bc_opt= 'ps_bc_grad00'
    elif methkey == "SPLINE":
        itype = 'ps_spline'
        bc_opt= 'ps_bc_dflt'
    elif methkey == "SPLINE_00":
        itype = 'ps_spline'
        bc_opt= 'ps_bc_grad00'
    elif methkey == "HERMITE_EXPLICIT":
        itype = 'ps_hermite'
        bc_opt= 'ps_bc_dflt'
    else:
        print(" methkey = ",methkey)
        print(" factor = ",factor)
        print(" interp_flags does not recognize... ")
        raise methkey_error

    return [itype,bc_opt]

def sublist_gen(iilist):

    # break passed list into (nsplit_sce) roughly equally sized sublists;
    # return lists of sublists

    isublists = []

    inum_tot = len(iilist)
    inum_sub = (inum_tot + nsplit_sce//2)//nsplit_sce

    indx = 0
    icount_sub = 0

    loclist = []

    for key in iilist:
        save_key = key
        loclist.append(key)
        indx = indx + 1
        if ( indx == inum_sub  and  icount_sub < nsplit_sce - 1 ):
            isublists.append(loclist)
            loclist = []
            indx = 0
            icount_sub = icount_sub + 1

    # complete the list of lists...

    isublists.append(loclist)

    ### print(" sublist_gen:")
    ### print("   inum_tot = ",inum_tot)
    ### print("   inum_sub = ",inum_sub)
    ### print("   indx = ",indx)
    ### print("   icount_sub = ",icount_sub)
    ### print("     loclist: ",loclist)
    ### print("     save_key: ",save_key)
    ### print("     len(isublists) = ",len(isublists))
    ### print("     nsplit_sce = ",nsplit_sce)

    return isublists
