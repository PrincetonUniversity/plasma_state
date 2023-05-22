#!/usr/bin/env python3

# --------------------------------------------------------------------------
#  plasma_state_global ... invoke with "from plasma_state_global import *"
#  these are quantities shared amongst the components of plasma_state
#
#  the following is read from the specification file:

version_id = "?.?"

#  array dimensions -- max list size -- set here.

maxlist = 1000

#  lengths of character strings in the state
#  (name) N -> 32; (file) F -> 256 assumed to be present always.

char_lens = [32, 256]

#  number of auxilliary hash code pairs
#  (hand maintained hash codes)

num_aux_hash_pairs = 9

#  number of subroutines into which to split large generated source codes

nsplit_sce = 10

# ----------------------------------------------------------
#  Reserved names -- the following names should not be used
#  in the plasma state specification

reserved_names = ["G", "P", "Q", "R", "Z", "PSI", "PSI_RZ",
                  "PHITOR", "GS_ERROR", "MAG_AXIS", "BDY_RZMINMAX",
                  "BR", "BR_RZ", "BZ", "BZ_RZ", "BPHI", "BPHI_RZ",
                  "BMOD", "BMOD_RZ"]

# ----------------------------------------------------------
#  sections (excluding header, constants, and components list) of the
#  specification file:
all_sections = []

#  list of defined components, set in plasma_state_components
all_components = {}

#  dictionary of defined constants [type,value,cmts]
#  all names start with "PS_"...
#    --> PARAMETER statements in f90; #define in C
all_constants = {}
all_constants_order = []

#  combined list of all dimensions...
all_dims = []

#  list dimension name variables
all_dim_namvars = []

#  dictionary: enumeration dimensions, with supplemental index labeling
#  information
all_enum_dims = {}

#  save list of enumeration limit variables
enum_maxlist = []

#  combined list of all profiles...
all_profiles = []

#  dictionary, all items...
#  added to by several procedures in plasma_state.py
all_items = {}

#  all groups...
all_groups = {}

#  all coordinates -- and corresponding physical units
all_coords = {}

#  list all dimension types: all_coords + ["S","L","I"]
known_dimtypes = []

#  various lists of items by category

species_list_items = []
species_list_attrib = []
item_list_items = []
grid_items = []

#  derivative profiles
deriv_list = []

#  machine description (not including the above)
mdescr = []

#  machine description namelist data:
#  include all declarations; preserve order as given in specification file
mdescr_namel = []

#  shot configuation (not including the above)
sconfig = []

#  shot configuration namelist data:
#  include all declarations; preserve order as given in specification file
sconfig_namel = []

#  simulation initialization data (not including the above)
simcon = []

#  state data (not including the above)
state_data_list = []

#  profiles (not including derivatives)
profile_list = []

#  list of all non-temporary files -- set here.
flist = []

#  list of f90 non-temporary files -- set here.
f90list = []

# list of C files
clist = []

# --------------------------------------------------------------------------
#  output files -- write generic header lines

f_const = open("plasma_state_constants_mod.f90", "w")
flist.append(f_const)
f90list.append(f_const)

f_util = open("plasma_state_util_mod.f90", "w")
flist.append(f_util)
f90list.append(f_util)

f_sdef = open("plasma_state_definition_mod.f90", "w")
flist.append(f_sdef)
f90list.append(f_sdef)

f_index = open("plasma_state_index.txt", "w")
flist.append(f_index)

f_cindx = open("ps_component_index.txt", "w")
flist.append(f_cindx)

f_xput = open("plasma_state_store.f90", "w")
flist.append(f_xput)
f90list.append(f_xput)

#  plasma_state_store subroutines...
f_xputa = []

f_xload = open("plasma_state_xload.f90", "w")
flist.append(f_xload)
f90list.append(f_xload)

f_xget = open("plasma_state_get.f90", "w")
flist.append(f_xget)
f90list.append(f_xget)

#  plasma_state_get subroutines...
f_xgeta = []

f_xcopy = open("plasma_state_copy.f90", "w")
flist.append(f_xcopy)
f90list.append(f_xcopy)

#  plasma_state_copy subroutines...
f_xcopa = []

f_merge = open("plasma_state_merge.f90", "w")
flist.append(f_merge)
f90list.append(f_merge)

#  plasma_state_merge subroutines...
f_merga = []

f_prune = open("plasma_state_prune_grids.f90", "w")
flist.append(f_prune)
f90list.append(f_prune)

f_ahash = open("plasma_state_hash_addr.f90", "w")
flist.append(f_ahash)
f90list.append(f_ahash)

f_hashc = open("plasma_state_hash_code.f90", "w")
flist.append(f_hashc)
f90list.append(f_hashc)

f_hashd = open("plasma_state_hash_diff.f90", "w")
flist.append(f_hashd)
f90list.append(f_hashd)

f_alloc = open("plasma_state_empty_alloc.f90", "w")
flist.append(f_alloc)
f90list.append(f_alloc)

# plasma_state_empty_alloc subroutines...
f_alla = []

f_clear = open("plasma_state_clear_profs.f90", "w")
flist.append(f_clear)
f90list.append(f_clear)

f_free = open("plasma_state_free.f90", "w")
flist.append(f_free)
f90list.append(f_free)

f_erase = open("plasma_state_erase.f90", "w")
flist.append(f_erase)
f90list.append(f_erase)

f_zonwt = open("plasma_state_zonwt.f90", "w")
flist.append(f_zonwt)
f90list.append(f_zonwt)

f_pscon = open("ps_contents_alloc.f90", "w")
flist.append(f_pscon)
f90list.append(f_pscon)

f_pscon2 = open("plasma_state_contents.f90", "w")
flist.append(f_pscon2)
f90list.append(f_pscon2)

f_pscon3 = []

f_wmd = open("ps_mdescr_namelist_write.f90", "w")
flist.append(f_wmd)
f90list.append(f_wmd)

f_wsc = open("ps_sconfig_namelist_write.f90", "w")
flist.append(f_wsc)
f90list.append(f_wsc)

f_rmd = open("ps_mdescr_namelist_read.f90", "w")
flist.append(f_rmd)
f90list.append(f_rmd)

f_rsc = open("ps_sconfig_namelist_read.f90", "w")
flist.append(f_rsc)
f90list.append(f_rsc)

f_uprd = open("ps_update_file_read.f90", "w")
flist.append(f_uprd)
f90list.append(f_uprd)

#  ps_update_file_read subroutines...
f_uprda = []

f_upwr = open("ps_update_file_write.f90", "w")
flist.append(f_upwr)
f90list.append(f_upwr)

#  ps_update_file_write subroutines...
f_upwra = []

f_uprp = open("ps_update_report_exec.f90", "w")
flist.append(f_uprp)
f90list.append(f_uprp)

# ----
# simplified portable structure -- single machine description dataset
# as collection of primitive types in a public module

f_mdmod = open("mdescr_data.f90", "w")
flist.append(f_mdmod)
f90list.append(f_mdmod)

f_mdinit = open("mdescr_init.f90", "w")
flist.append(f_mdinit)
f90list.append(f_mdinit)

f_mdread = open("mdescr_read.f90", "w")
flist.append(f_mdread)
f90list.append(f_mdread)

f_pspcat = open("ps_print_category.f90", "w")
flist.append(f_pspcat)
f90list.append(f_pspcat)

# ----

# c_getset = open("ccps_getset.f90", "w")
# flist.append(c_getset)
# clist.append(c_getset)

# --------------------------------------------------------------------------
