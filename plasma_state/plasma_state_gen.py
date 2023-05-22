#!/usr/bin/env python3

#
# Python code generator for the Plasma State Component
#    D. McCune (dmc) Sept 2006
#
# version 2.0 -- Nov. 2007
#
# This is a code generator.
# This python script READS "plasma_state_spec.dat"
# and WRITES:
#
#     plasma_state_definition_mod.f90       ! generic state definition module
#     plasma_state_index.txt                ! alphabetized list of var. names
#
# The module will contain the following public routines:
#
#     ps_get_plasma_state(ierr)             ! read current state from file
#     ps_store_plasma_state(ierr)           ! write current state to file
#     ps_commit_plasma_state(ierr)          ! copy current state -> prior state
#          ! this refers to both the files and the memory copies
#
# and also writes the following semi-private routines to aid implementation:
#
#       plasma_state_get.f90
#       subroutine plasma_state_get(filename,ss,ierr)
#            character*(*), intent(in) :: filename        ! state file
#            type (plasma_state) :: ss     ! state data to read
#            integer, intent(out) :: ierr  ! completion code (0=OK)
#
#       plasma_state_store.f90
#       subroutine plasma_state_get(filename,ss,ierr)
#            character*(*), intent(in) :: filename        ! state file
#            type (plasma_state) :: ss     ! state data to write
#            integer, intent(out) :: ierr  ! completion code (0=OK)
#
#       plasma_state_copy.f90
#       subroutine plasma_state_copy(ss1,ss2,ierr)
#            type (plasma_state) :: ss1    ! state object from which to copy
#            type (plasma_state) :: ss2    ! state object to which to copy
#
#
# NB DMC July 2007 -- the above description is incomplete...

# --------------------------------------------------------------------------
# global items...

from plasma_state_global import *

global mdescr
global version_id

global f_merga
global f_xcopa
global f_xgeta
global f_xputa

global flist
global f90list

# --------------------------------------------------------------------------
# the parsing of the state specification is done in subfunctions
# plasma_state_sub...

from plasma_state_sub import *

# --------------------------------------------------------------------------
# the

from plasma_state_write import *

# --------------------------------------------------------------------------
#  define known coordinates (values are units) and all dimension types

all_coords["RHO"] = "-"
all_coords["TH"] = "rad"
all_coords["R"] = "m"
all_coords["Z"] = "m"
all_coords["E"] = "keV"
all_coords["KY"] = "-"

known_dimtypes.append("RHO")
known_dimtypes.append("TH")
known_dimtypes.append("R")
known_dimtypes.append("Z")
known_dimtypes.append("E")
known_dimtypes.append("KY")

known_dimtypes.append("S")  # species list
known_dimtypes.append("L")  # item list
known_dimtypes.append("I")  # generic integer

# --------------------------------------------------------------------------
#  READ the specification file
#
f = open("plasma_state_spec.dat", "r")
lines = f.readlines()
f.close()

# --------------------------------------------------------------------------
# first standardization of input text

clines = []

for line in lines:

    # throw out # comments, leading/trailing whitespace, and blank lines
    # also look for Version_ID.

    line = line.strip()

    if len(line) == 0:
        continue

    if line[0] != "#":

        # check for Version_ID if not already seen:

        if version_id == "?.?":
            vchk = line.split(":")
            if len(vchk) == 2:
                if vchk[0].strip() == "Version_ID":
                    version_id = vchk[1].strip()
                    continue

        # if an "!" (fortran) comment is present, put it on a separate line

        ifind = line.find("!")
        if ifind > 0:
            cline = line[ifind:]
            line = line[:ifind].strip()

            clines.append(line)
            clines.append(cline)

        else:
            clines.append(line)

if version_id == "?.?":
    print(" ?plasma_state_gen.py: Version_ID: ",
          "not found in plasma_state_spec.dat")
    raise version_id_error
else:
    print(" plasma_state_gen: processing Plasma State definition version: " +
          version_id)

# --------------------------------------------------------------------------
#  output files (see plasma_state_global) -- write generic header lines
#  also, start sub-subroutines, plasma_state_merge_1.f90, _2.f90, etc...

count = 0
while (count < nsplit_sce):
    fnam = "plasma_state_merge_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_merga.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "plasma_state_copy_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_xcopa.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "plasma_state_get_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_xgeta.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "plasma_state_store_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_xputa.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "ps_update_file_read_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_uprda.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "ps_update_file_write_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_upwra.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "plasma_state_empty_alloc_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_alla.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    fnam = "plasma_state_contents_profs_" + repr(count) + ".f90"
    ftmp = open(fnam, "w")
    f_pscon3.append(ftmp)
    flist.append(ftmp)
    f90list.append(ftmp)

    count = count + 1

for f in flist:
    f.write("! written by plasma_state_gen.py\n")
    f.write("!   input: plasma_state_spec.dat, version id: " +
            version_id + "\n")
    f.write("! *** do not edit *** (use code generator instead)\n")
    f.write("\n")

# -------------------------------------------------------------------------
# split into sections; all lines must be in labeled sections, started by
# a line of the form "Section: <section-name>".

iline = 0
current_section = "NONE"
clines.append("SECTION: END")

for line in clines:

    iline = iline + 1

    words = line.split()
    word0 = words[0].upper()

    if word0 == "SECTION:":
        if current_section == "NONE":
            if iline > 1:
                print('plasma_state_gen.py: line before ',
                      '"SECTION:" specification.')
                print(line)
                raise plasma_state_error_exit

        elif current_section == "END":
            pass

        else:
            # before reassigning current_section, process the preceding section

            if current_section == "HEADER":
                hlines = slines
                # will be used in plasma_state_proc_header...

                # plasma_state_proc_header writes the declaration header
                # of each output generated source file

            elif current_section == "COMPONENTS":
                plasma_state_components(slines, current_section)

                # this reads the list of components

            elif current_section == "CONSTANTS":
                plasma_state_constants(slines, current_section)

                # this reads the list of defined constants

            elif current_section == "MACHINE_DESCRIPTION":
                all_sections.append(current_section)
                plasma_state_vars(slines, current_section)

                # this reads the list of machine description variables
                # (no profiles)

            elif current_section == "SHOT_CONFIGURATION":
                all_sections.append(current_section)
                plasma_state_vars(slines, current_section)

                # this reads the list of shot configuration variables
                # (no profiles)

            elif current_section == "SIMULATION_INIT":
                all_sections.append(current_section)
                plasma_state_vars(slines, current_section)

                # this reads the list of simulation initialization variables
                # (no profiles)

            elif current_section == "STATE_DATA":
                all_sections.append(current_section)
                plasma_state_vars(slines, current_section)

                # this reads the list of time dependent state variables
                # (no profiles)

            elif current_section == "STATE_PROFILES":
                all_sections.append(current_section)
                plasma_state_profiles(slines, current_section)

                # this reads the list of time dependent profiles

            else:
                print('plasma_state_gen.py: unknown section name: ',
                      current_section)
                raise plasma_state_error_exit

        if len(words) != 2:
            print('plasma_state_gen.py: expected syntax: ',
                  '"SECTION: <section-name>"')
            print(line)
            raise plasma_state_error_exit

        current_section = words[1].upper()

        print(current_section)

        slines = []

    else:

        # here the lines for the current section are accumulated...

        slines.append(line)

# put some lists in alphabetic order

all_dims.sort()
all_profiles.sort()
all_dim_namvars.sort()

species_list_items.sort()

for key in list(all_items.keys()):
    idescr = all_items[key]
    section = idescr['section']
    iattrib = 0
    if 'dlist' in idescr and section == "SIMULATION_INIT" and key != "FRACMIN":
        dlist = idescr['dlist']
        iattrib = 1
        for dpair in dlist:
            dkey = dpair[1].upper()
            if dkey not in species_list_items:
                iattrib = 0
    if iattrib == 1:
        if key not in species_list_attrib:
            # print " --> ",key," ",idescr['section']
            species_list_attrib.append(key)

species_list_attrib.sort()

item_list_items.sort()
grid_items.sort()

deriv_list.sort()

mdescr.sort()
sconfig.sort()
simcon.sort()
state_data_list.sort()

# difference btw all_profiles & profile_list: all_profiles includes derivatives

profile_list.sort()

# -----------------------------
# debug...

# print len(species_list_items)
# print " species_list_items = ",species_list_items
# print len(species_list_attrib)
# print " species_list_attrib = ",species_list_attrib
# print " item_list_items = ",item_list_items
# print all_enum_dims

# print all_dim_namvars

# print all_items['RLIM']
# print all_items['ZLIM']
# print "\n all_coords: ",all_coords

print("")
print(" total number of state elements: ", len(list(all_items.keys())))
print("")

# -----------------------------
# generate output files and add finishing touches

plasma_state_proc_header(version_id, hlines, "HEADER")

plasma_state_write()

from plasma_cwrite import plasma_cwrite
plasma_cwrite(version_id)

# -------------------------------------------------------------------------
#  output files -- close

for f in flist:
    f.close()

# -------------------------------------------------------------------------
print('plasma_state_gen.py:  processing completed.')
