#!/usr/bin/env python3

#  plasma_state_sub.py
#
#  subfunctions of plasma_state_gen.py, handling aspects of code generation
#  for the plasma state software.
#
###
#  The following procudures process sections of the specification file:
#
#     plasma_state_proc_header
#        (this routine also inserts version ID and header information
#        into generated source routines).
#
#     plasma_state_components
#     plasma_state_constants
#     plasma_state_vars
#     plasma_state_profiles
#
#  These routines ONLY read the specification file and accumulate information
#  on the specification.  When this is complete,
#
#     plasma_state_write
#
#  controls the process of writing the generated code.
#
#  the syntax of the specification file is described in the comments of
#  "plasma_state_spec.dat"
#
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# global items...

from plasma_state_global import *

#--------------------------------------------------------------------------
# process header data
# ...moved to plasma_state_write.py...
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#  process component definitions-- acquire list of component names...
#  all state elements must belong to a specific component
#
#  specification syntax:
#     <component-name>

def plasma_state_components(text,section):

    global all_components

    cur_component = "??"
    cmts = []

    loclist = []

    for line in text:

        if line[0] == "!":
            if cur_component == "??":
                swimgen_error(line,section," !-comment precedes first item")
            else:
                acmt = line[1:].strip()  # remove leading "!"
                cmts.append(acmt)

        else:
            if cur_component != "??":
                if cur_component in loclist:
                    swimgen_error(line,section,
                                  "in preceding line: component already specified: " + cur_component)
                if len(cmts) == 0:
                    swimgen_error(line,section,
                                  "comments not found for component: " + cur_component)
                loclist.append(cur_component)
                all_components[cur_component] = cmts
                cur_component = "??"
                cmts=[]

            words = line.split()
            if len(words) > 1:
                print(" ?swimgen_components: too many words in line.")
                swimgen_error(line,section,"expected syntax:  <component-name> [!cmt]")

            cur_component = words[0].upper()

    #----------------
    #  pick up last component...

    if cur_component != "??":
        if cur_component in loclist:
            swimgen_error(line,section,
                          "in preceding line: component already specified: " + cur_component)
        if len(cmts) == 0:
            swimgen_error(line,section,
                          "comments not found for component: " + cur_component)
        loclist.append(cur_component)
        all_components[cur_component] = cmts
        cur_component = "??"
        cmts = []

    idebug = "FALSE"

    if idebug == "TRUE":
        clist = list(all_components.keys())
        clist.sort()
        print(" plasma_state_components debug output: ")
        for cur_component in clist:
            cmts = all_components[cur_component]
            print(cur_component)
            for cmt in cmts:
                print("      " + cmt)

#--------------------------------------------------------------------------
#  list of named constants (in f90, PARAMETERs, scalar values, I & R).
#
#   <type> <name> = <value>
#   all names to start with "ps_".

def plasma_state_constants(text,section):

    global all_constants
    global all_constants_order

    cur_key = "??"
    cmts = []

    #---------------------------------------------------
    #  PARSE input text; check for errors

    for line in text:

        if line[0] == "!":
            if cur_key == "??":
                swimgen_error(line,section," !-comment precedes first item")
            else:
                cmts.append(line[1:].strip())  # remove leading "!"

        else:

            if cur_key != "??":
                if cur_key in all_constants:
                    swimgen_error(line,section,
                                  ' duplicate name for constant: ' + cur_key)

                all_constants[cur_key] = [ftype,rhs,cmts]
                all_constants_order.append(cur_key)
                cur_key = "??"
                cmts = []

            sides = line.split('=')
            if len(sides) != 2:
                swimgen_error(line,section,
                              " syntax error, exactly one = sign expected")

            lhs_pair = sides[0].split()
            if len(lhs_pair) != 2:
                swimgen_error(line,section,
                              " syntax error; <type> <name> expected on LHS")

            rhs = sides[1].strip()

            dtype = lhs_pair[0].upper()

            if dtype != "R" and dtype != "I":
                swimgen_error(line,section,
                              " only R and I type constants are supported.")

            ftype = standard_dtype(dtype,line,section)

            name = lhs_pair[1]
            cur_key = name.upper()
            check_key(cur_key,line,section)

            if cur_key[0:3] != "PS_":
                swimgen_error(line,section,
                              ' named constant must start with prefix "ps_".')

    if cur_key != "??":
        if cur_key in all_constants:
            swimgen_error(line,section,
                          ' duplicate name for constant: ' + cur_key)

        all_constants[cur_key] = [ftype,rhs,cmts]
        all_constants_order.append(cur_key)

#--------------------------------------------------------------------------
#  generate groups of scalars and non-profile arrays in the state
#  (various sections).  In these sections, array dimension symbols are
#  also implicitly defined.
#
#  see corresponding section of plasma_state_state_spec.dat file

def plasma_state_vars(text,section):

    global all_dims
    global all_dim_namvars
    global all_enum_dims

    global all_items
    global all_groups

    global enum_maxlist

    global species_list_items
    global species_list_attrib
    global item_list_items
    global grid_items

    #--------------------------------------------
    #  determine which types of array dimensions can be defined on this call...
    #     (enumeration dimension rules handled elsewhere).

    ilist_enable = "FALSE"   # item list
    slist_enable = "FALSE"   # species list
    grid_enable = "FALSE"    # grid

    maxrank = 3

    if section == "MACHINE_DESCRIPTION":
        ilist_enable = "TRUE"
        maxrank = 1

    elif section == "SHOT_CONFIGURATION":
        slist_enable = "TRUE"
        ilist_enable = "TRUE"
        maxrank = 1

    elif section == "SIMULATION_INIT":
        grid_enable = "TRUE"
        slist_enable = "TRUE"
        ilist_enable = "TRUE"
        maxrank = 1

    # print " ilist_enable = ",ilist_enable
    # print " slist_enable = ",slist_enable
    # print " grid_enable = ",grid_enable

    units = "??"
    cur_key = "??"
    cur_component = "??"
    cur_group = "??"
    cur_group_units = "??"
    cur_group_type = "??"
    cmts = []
    slist_flag = "FALSE"
    item_flag = "FALSE"

    idescr = {}  # description of item

    loc_enum_dims = {}
    #---------------------------------------------------
    #  PARSE input text; check for errors

    for line in text:

        if line[0] == "!":
            if cur_key == "??":
                swimgen_error(line,section," !-comment precedes first item")
            else:
                acmt = line[1:].strip()  # remove leading "!"
                cmts.append(acmt)

        else:
            if cur_key != "??":
                if len(cmts) == 0:
                    swimgen_error(line,section,
                                  "comments not found for variable: " + cur_key)
                idescr['cmts'] = cmts
                all_items[cur_key] = idescr

                splist_update(item_flag,cur_key,section,"add")

                cur_key = "??"
                idescr = {}  # description of next item
                cmts = []
                slist_flag = "FALSE"
                item_flag = "FALSE"

            words = line.split()
            word0 = words[0].upper()

            if word0 != "COMPONENT:":
                if cur_component == "??":
                    swimgen_error(line,section,
                                  'component designation must precede group and item definitions.')

            if word0 == "COMPONENT:":
                if len(words) != 2:
                    swimgen_error(line,section,'syntax: "component: <component>"')

                cur_component = words[1].upper()
                check_key(cur_component,line,section)

                if cur_component not in all_components:
                    swimgen_error(line,section,'reference to undefined component: ' + cur_component)

            elif word0 == "GROUP:":
                if len(words) != 2:
                    swimgen_error(line,section,
                                  'syntax: "group: <group_name>[|units=<units>]"')

                gdef = words[1].split('|')
                if len(gdef) > 2:
                    swimgen_error(line,section,
                                  'syntax: "group: <group_name>[|units=<units>]"')
                cur_group = gdef[0].upper()
                check_key(cur_group,line,section)
                dupcheck(section,cur_group)

                all_groups[cur_group] = [cur_component]

                if len(gdef) == 2:
                    # units specification
                    cur_group_units = uparse(gdef[1],line,section)

            elif word0 == "END_GROUP":
                cur_group = "??"
                cur_group_units = "??"
                cur_group_type = "??"

            else:
                # an item definition...

                if len(words) < 2:
                    swimgen_error(line,section,
                                  'syntax: <type> <variable> expected.')

                iattrib_slist = 0

                #  separate variable name from dimensions (if any)
                var_decl = words[1].split('(')
                name = var_decl[0]

                nldim = ""
                iwd = 1
                while iwd < len(var_decl):
                    nldim = nldim + var_decl[iwd]
                    iwd = iwd + 1
                    if iwd < len(var_decl):
                        nldim = nldim + "("

                if len(var_decl) > 1:
                    var_decl = [name,nldim]

                #  separate variable type from qualifiers (if any)
                ddecl = words[0].split('|')

                #  variable type
                dtype = ddecl[0].upper()

                #  check for ENUM qualifier
                enum_decl = '(none)'

                if dtype == "I" and len(ddecl) == 2:
                    enum_decl = ddecl[1].upper()
                    ddecl = []
                    ddecl.append(dtype)
                    if enum_decl[0:4] != "ENUM":
                        swimgen_error(line,section,
                                      'unrecognized qualifier for "I" element: ' + enum_decl)
                    if enum_decl == "ENUM":
                        enum_decl = '(index)'
                    elif enum_decl[4:5] != ":":
                        swimgen_error(line,section,
                                      'malformed "ENUM" qualifer: required syntax is "ENUM" or "ENUM:<name>"')
                    else:
                        enum_decl = enum_decl[5:].upper()

                    all_enum_dims[name.upper()] = enum_decl

                if len(var_decl) < 2:
                    is_scalar = "TRUE"
                    if dtype == "IS":
                        iattrib_slist = 1
                        dtype = "I"
                else:
                    is_scalar = "FALSE"
                    if nldim[-1:] != ")":
                        swimgen_error(line,section,
                                      'trailing parentheses not found in dimension declaration.')
                    nldim = nldim[:-1].upper()

                if section == "MACHINE_DESCRIPTION":
                    mdescr_namel.append([dtype,name,nldim])

                elif section == "SHOT_CONFIGURATION":
                    sconfig_namel.append([dtype,name,nldim])

                if cur_group != "??":
                    #  group affiliation active: only scalars can be present

                    if len(ddecl) > 1:
                        swimgen_error(line,section,
                                      '"units" or other "|" qualifier not valid within scalar group structure.')

                    if enum_decl != '(none)':
                        swimgen_error(line,section,
                                      'ENUM dimension declaration cannot be inside a group.')
                    if is_scalar == "FALSE":
                        swimgen_error(line,section,
                                      'scalars only inside group structure.')
                    dtype = word0.upper()
                    if dtype == "IS":
                        dtype = "I"

                    if dtype == "L" or dtype == "S" or dtype == "G":
                        swimgen_error(line,section,
                                      'Compound type "' + dtype +'" declaration must not occur inside a group structure.')

                    ftype = standard_dtype(dtype,line,section)

                    if len(words) > 2:
                        if len(words) != 4 or words[2] != "=":
                            print(" words = ",words)
                            swimgen_error(line,section,
                                          'syntax error; expect <type> <name> [= <init_value>]')
                        init = words[3]
                    else:
                        if dtype == "R":
                            init = "0.0_rspec"
                        elif dtype == "I":
                            init = "ps_unInit"
                        else:
                            init = '" "'

                    if cur_group_type == "??":
                        cur_group_type = dtype
                    else:
                        if cur_group_type != dtype:
                            swimgen_error(line,section,
                                          "all elements within group need to be of the same type.")

                    if dtype == "R":
                        if cur_group_units == "??":
                            swimgen_error(line,section,
                                          'groups with type "R" elements must include a units specification.')

                    else:
                        if cur_group_units != "??":
                            print(" --> cur_group_units = ",cur_group_units)
                            swimgen_error(line,section,
                                          'groups with type elements other than "R" must NOT include units specification.')

                    cur_key = name.upper()
                    check_key(cur_key,line,section)
                    dupcheck(section,cur_key)

                    scalar_descr(name,dtype,ftype,init,cur_group,
                                 cur_group_units,cur_component,line,section,
                                 idescr)

                    all_groups[cur_group].append(cur_key)
                elif is_scalar == "TRUE":

                    if dtype == "L" or dtype == "S" or dtype == "G":
                        swimgen_error(line,section,
                                      'Compound type "' + dtype +'" declaration requires an array syntax.')

                    ftype = standard_dtype(dtype,line,section)

                    units = "??"

                    if dtype == "R":
                        if len(ddecl) == 2:
                            units = uparse(ddecl[1],line,section)
                        elif len(ddecl) > 2:
                            swimgen_error(line,section,
                                          'too many "|" qualifiers in standalone scalar specification.')
                        else:
                            swimgen_error(line,section,
                                          'standalone scalar units specification missing.')
                    else:
                        if len(ddecl) > 1:
                            swimgen_error(line,section,
                                          'unexpected "|" qualifier')

                    if len(words) > 2:
                        if len(words) != 4 or words[2] != "=":
                            print(" words = ",words)
                            swimgen_error(line,section,
                                          'syntax error; expect <type> <name> [= <init_value>]')
                        init = words[3]
                    else:
                        if dtype == "R":
                            init = "0.0_rspec"
                        elif dtype == "I":
                            init = "ps_unInit"
                        else:
                            init = '" "'

                    cur_key = name.upper()
                    check_key(cur_key,line,section)
                    dupcheck(section,cur_key)

                    scalar_descr(name,dtype,ftype,init,cur_group,
                                 units,cur_component,line,section, idescr)

                    # check for enumeration scalar array dimension declaration
                    # if so, add dimension fields to dictionary entry

                    if enum_decl != '(none)':
                        idescr['dimlabel'] = 'enum'

                        idescr['dimtype'] = "I"
                        idescr['dim_lval'] = "1"
                        idescr['ilist'] = [0]
                        idescr['init'] = "0"

                        all_dims.append(cur_key)

                else:
                    #  array specification
                    #  can also implicitly define an array dimension

                    if len(words) > 2:
                        swimgen_error(line,section,
                                      'array declaration syntax: <type-code>[|qualifier] <array_name>([dimensions,...])')

                    dimtype = "NONE"
                    orientation = "NONE"
                    if dtype == "L":
                        if ilist_enable == "FALSE":
                            swimgen_error(line,section,
                                          '"L" (item list) data type not supported here.')
                        if len(ddecl) != 2:
                            swimgen_error(line,section,
                                          '(item list declaration syntax: L|<label>')
                        dimtype = "L"
                        dlabel = ddecl[1]
                        dimlist = dim_define(var_decl,line,section)
                        dim_lval = dimlist[0]
                        dimname = dimlist[1].lower()

                        dcmt = "item list dimension of " + name
                        dcmt = dcmt + " (" + dlabel + ")"

                        # corresponding array to be defined is of type "N"

                        ddecl = ["N"]
                        dtype = ddecl[0]

                        item_flag = "LIST"

                    if dtype == "S":
                        if slist_enable == "FALSE":
                            swimgen_error(line,section,
                                          '"S" (species list) data type not supported here.')
                        if len(ddecl) != 2:
                            swimgen_error(line,section,
                                          '(species list declaration syntax: S|<label>')
                        dimtype = "S"
                        dlabel = ddecl[1]
                        dimlist = dim_define(var_decl,line,section)
                        dim_lval = dimlist[0]
                        dimname = dimlist[1].lower()

                        dcmt = "species list dimension of " + name
                        dcmt = dcmt + " (" + dlabel + ")"

                        ddecl = ["N"]
                        dtype = ddecl[0]

                        slist_flag = "TRUE"
                        slist_name = name

                        item_flag = "SPECIES_LIST"

                        # corresponding array to be defined is of type "N"
                        # this definition is deferred; pick up comments...

                        sroot = name
                        sname = name + "_name"
                        new_decl = [sname,var_decl[1]]
                        var_decl = new_decl

                        name = sname

                    if dtype == "G":
                        if grid_enable == "FALSE":
                            swimgen_error(line,section,
                                          '"G" (grid) data type not supported here.')
                        nparts = name.split("_")
                        coord = nparts[0].upper()
                        if coord not in all_coords:
                            swimgen_error(line,section,
                                          '"G" declaration: name "' + name + '" could not be mapped to known coordinate.')

                        if coord == "TH":
                            if len(ddecl) != 2:
                                swimgen_error(line,section,
                                              '"TH" grid requires G|CW or G|CCW specification.')
                            orientation = ddecl[1].upper()
                        else:
                            if len(ddecl) != 1:
                                swimgen_error(line,section,
                                              '"G" declaration, non-periodic grid: no "|" qualifier allowed.')

                        dimtype = coord
                        dlabel = coord
                        dimlist = dim_define(var_decl,line,section)
                        dim_lval = dimlist[0]
                        dimname = dimlist[1].lower()

                        dcmt = "grid dimension of " + name
                        dcmt = dcmt + " (" + dlabel + " coordinate)"

                        # corresponding array of type "R" with units:

                        dstr = "R|units=" + all_coords[coord]
                        ddecl = dstr.split("|")
                        dtype = ddecl[0]

                        item_flag = "GRID"

                    if dimtype != "NONE":
                        cur_dimkey = dimname.upper()
                        check_key(cur_dimkey,line,section)
                        dupcheck(section,cur_dimkey)

                        ftype = standard_dtype("I"," "," ")

                        ddescr = {}
                        scalar_descr(dimname,"I",ftype,"0",cur_group,
                                 "??",cur_component,line,section, ddescr)

                        ddescr['dimlabel'] = dlabel
                        ddescr['dimtype'] = dimtype
                        ddescr['dim_lval'] = dim_lval
                        ddescr['ilist'] = [0]

                        if dimtype in all_coords:
                            ddescr['gridname'] = name
                        elif dimtype == "L" or dimtype == "S":
                            ddescr['name_var'] = name
                            all_dim_namvars.append(name.upper())

                        if dimtype == "S":
                            ddescr['rootname'] = sroot

                        if orientation != "NONE":
                            ddescr['orientation'] = orientation
                        dcmts = []
                        dcmts.append(dcmt)
                        ddescr['cmts'] = dcmts

                        all_items[cur_dimkey] = ddescr
                        all_dims.append(cur_dimkey)

                        splist_update(item_flag,cur_dimkey,section,"add")

                    [enum_code,enum_codim,ienum,dlist] = get_dimlist(var_decl[1],cur_component,line,section)
                    maxrank_adj = adjust_maxrank(maxrank,dlist,ienum)

                    if len(dlist) > maxrank_adj:
                        print(dlist)
                        print(' in section ' + section + ' maxRank = ', maxrank)
                        swimgen_error(line,section,
                                      ' maximum array rank exceeded.')

                    if slist_flag == "TRUE":
                        # define data arrays associated with species list...
                        # add "slist" attribute = root name
                        # to dimkey add "snames" entry -- list of derived names

                        all_items[cur_dimkey]['snames'] = [sname]

                        sname = slist_name + "_type"
                        all_items[cur_dimkey]['snames'].append(sname)
                        ktype = "I"
                        ftype = standard_dtype(ktype," "," ")
                        init = "0"

                        slist_array_descr(slist_name,sname,"type",
                                          ktype,ftype,init,cur_group,"??",
                                          cur_component,line,section,dlist)

                        ktype = "R"
                        ftype = standard_dtype(ktype," "," ")
                        init = "0.0_rspec"

                        sname = "q_" + slist_name
                        all_items[cur_dimkey]['snames'].append(sname)
                        units = "C"

                        slist_array_descr(slist_name,sname,"q",
                                          ktype,ftype,init,cur_group,units,
                                          cur_component,line,section,dlist)

                        sname = "qatom_" + slist_name
                        all_items[cur_dimkey]['snames'].append(sname)
                        units = "C"

                        slist_array_descr(slist_name,sname,"qatom",
                                          ktype,ftype,init,cur_group,units,
                                          cur_component,line,section,dlist)

                        sname = "m_" + slist_name
                        all_items[cur_dimkey]['snames'].append(sname)
                        units = "kg"

                        slist_array_descr(slist_name,sname,"m",
                                          ktype,ftype,init,cur_group,units,
                                          cur_component,line,section,dlist)

                    if dtype == "R":
                        if len(ddecl) == 2:
                            units = uparse(ddecl[1],line,section)
                        elif len(ddecl) > 2:
                            swimgen_error(line,section,
                                          'too many "|" qualifiers in standalone scalar specification.')
                        else:
                            swimgen_error(line,section,
                                          'standalone scalar units specification missing.')
                    else:
                        if len(ddecl) > 1:
                            swimgen_error(line,section,
                                          'unexpected "|" qualifier')

                    if dtype == "R":
                        init = "0.0_rspec"
                    elif dtype == "I":
                        init = "0"
                    else:
                        init = '" "'

                    idescr = {}
                    ftype = standard_dtype(dtype,line,section)
                    array_descr(name,dtype,ftype,init,cur_group,units,
                                cur_component,line,section,dlist, idescr)

                    if enum_codim > 0:
                        idescr['enum_codim'] = enum_codim

                    cur_key = name.upper()
                    check_key(cur_key,line,section)
                    dupcheck(section,cur_key)

                    if enum_decl != '(none)':
                        if len(dlist) > 1:
                            swimgen_error(line,section,"enumeration dimension " + cur_key + " rank must not exceed 1.")
                        # integer vector enumeration (variable length)
                        idescr['dimlabel'] = 'enum_vec'

                        mline = '(derived from ' + cur_key + ')'

                        mxdkey = "MAX_" + cur_key
                        if mxdkey in all_items:
                            swimgen_error(line,section,"enumeration dimension implicitly defines " + mxdkey + ": duplicate name")

                        enum_maxlist.append(mxdkey)

                        mxdescr = {}

                        mxftype = standard_dtype("I"," "," ")
                        mxdimname = mxdkey.lower()
                        scalar_descr(mxdimname,"I",mxftype,"0",cur_group,
                                     "??",cur_component,mline,section, mxdescr)
                        mxdescr['dimlabel'] = 'enum_max'
                        mxdescr['dimtype'] = "MAX"
                        mxdescr['dim_lval'] = "1"
                        mxdescr['ilist'] = [0]
                        mxdescr['init'] = '0'

                        mxcmts = []
                        mxcmt = 'Maximum size of variable length enumeration: ' + cur_key
                        mxcmts.append(mxcmt)
                        mxdescr['cmts'] = mxcmts

                        all_items[mxdkey] = mxdescr
                        all_dims.append(mxdkey)

                        if section == "MACHINE_DESCRIPTION":
                            mdescr_namel.append(["I",mxdimname,""])

                        elif section == "SHOT_CONFIGURATION":
                            sconfig_namel.append(["I",mxdimname,""])

                if iattrib_slist == 1:
                    species_list_attrib.append(cur_key)


    if cur_key != "??":
        if len(cmts) == 0:
            swimgen_error(line,section,
                          "comments not found for variable: " + cur_key)
        idescr['cmts'] = cmts
        all_items[cur_key] = idescr
        splist_update(item_flag,cur_key,section,"add")

    # check enumeration labeling references
    #   I|ENUM:ifoo  dimfoo
    #      expects existence of symbol:  ifoo(dimfoo)
    #   I|ENUM:nphi  num_nphi(nicrf_src)
    #      expects existence of symbol:  nphi(max_num_nphi,nicrf_src)

    for ekey in list(all_enum_dims.keys()):
        ref = all_enum_dims[ekey]
        if ref != '(index)':
            rkey = ref.upper()
            if rkey not in all_items:
                print(' ?during parse of ' + section + ':')
                print('  enumeration dimension "' + ekey + '" references symbol "' + rkey + '".')
                print('  But, ' + rkey + ' cannot be found.')
                raise plasma_state_spec_error

            edescr = all_items[ekey]
            rdescr = all_items[rkey]
            if rdescr['dtype'] != "I":
                print(' ?during parse of ' + section + ':')
                print('  enumeration dimension "' + ekey + '" references symbol "' + rkey + '".')
                print('  But, ' + rkey + ' is not of type INTEGER as expected.')
                raise plasma_state_spec_error

            if 'dlist' not in rdescr:
                print(' ?during parse of ' + section + ':')
                print('  index array ' + rkey + ' for enumeration dimension ' + ekey)
                print('  should be dimensioned by ' + ekey + ' but is not.')
                raise plasma_state_spec_error

            rdims = rdescr['dlist']
            if 'dlist' not in edescr:
                e_err = 0
                if len(rdims) != 1:
                    e_err = 1
                else:
                    rpair = rdims[0]
                    rlower = rpair[0].upper()
                    rupper = rpair[1].upper()
                    if rlower != 'I' or rupper != ekey:
                        e_err = 1

                if e_err != 0:
                    print(' ?during parse of ' + section + ':')
                    print('  enumeration dimension "' + ekey + '" references symbol "' + rkey + '".')
                    print('  but this symbol is not dimensioned by ' + ekey + ' as expected.')
                    raise plasma_state_spec_error
            else:
                ddims = edescr['dlist']
                if len(ddims) != 1:
                    print(' ?during parse of ' + section + ':')
                    print('  enumeration dimension vector "' + ekey + '" has more than one dimension.')
                    raise plasma_state_spec_error
                e_err = 0
                if len(rdims) != 2:
                    e_err = 1
                else:
                    ekmax = "MAX_" + ekey
                    if rdims[0][0] != "I" or rdims[1][0] != ddims[0][0]:
                        e_err = 1
                    if rdims[0][1].upper() != ekmax:
                        e_err = 1
                    if rdims[1][1] != ddims[0][1]:
                        e_err = 1
                if e_err != 0:
                    print(' ?during parse of ' + section + ':')
                    print('  enumeration dimension "' + ekey + '" references symbol "' + rkey + '".')
                    print('  but this symbol is not dimensioned as expected:')
                    print(' ')
                    print('     (' + ekey.lower() + '(),' + ddims[0][1] + ')')
                    print(' ')
                    raise plasma_state_spec_error

#--------------------------------------------------------------------------
#  generate groups of profiles in plasma state, organized as lists.
#
#  see corresponding section of plasma_state_state_spec.dat file

def plasma_state_profiles(text,section):

    global all_items
    global all_groups

    #--------------------------------------------

    maxrank_total = 6  # max total rank of arrays
    maxrank_coord = 3  # max #of coordinate dimensions
    maxrank_list = 3   # max #of non-coordinate dimensions (e.g. species lists)

    cur_key = "??"
    cur_component = "??"
    cur_group = "??"
    cur_group_units = "??"
    cur_group_dimstr = "??"
    cmts = []

    idescr = {}  # description of item

    namtype = standard_dtype("N"," ",section)  # C*32
    idtype = standard_dtype("I"," ",section)   # Int

    hermlist = []  # list of HERMITE_EXPLICIT items
    # ( these will need derivative arrays)

    #---------------------------------------------------
    #  PARSE input text; check for errors

    for line in text:

        if line[0] == "!":
            if cur_key == "??":
                swimgen_error(line,section," !-comment precedes first item")
            else:
                acmt = line[1:].strip()  # remove leading "!"
                cmts.append(acmt)

        else:
            if cur_key != "??":
                if len(cmts) == 0:
                    swimgen_error(line,section,
                                  "comments not found for profile: " + cur_key)
                idescr['cmts'] = cmts
                all_items[cur_key] = idescr

                cur_key = "??"
                idescr = {}  # description of next item
                cmts = []

            words = line.split()
            word0 = words[0].upper()

            if word0 != "COMPONENT:":
                if cur_component == "??":
                    swimgen_error(line,section,
                                  'component designation must precede group and item definitions.')

            if word0 == "COMPONENT:":
                if len(words) != 2:
                    swimgen_error(line,section,'syntax: "component: <component>"')

                cur_component = words[1].upper()
                check_key(cur_component,line,section)

                if cur_component not in all_components:
                    swimgen_error(line,section,'reference to undefined component: ' + cur_component)

            elif word0 == "GROUP:":
                if len(words) != 2:
                    swimgen_error(line,section,
                                  'syntax: "group: <group_name>|units=<units>"')

                gdef = words[1].split('|')
                if len(gdef) != 2:
                    swimgen_error(line,section,
                                  'syntax: "group: <group_name>|units=<units>"')
                cur_group = gdef[0].upper()
                check_key(cur_group,line,section)
                dupcheck(section,cur_group)
                all_groups[cur_group] = [cur_component]

                # units specification
                cur_group_units = uparse(gdef[1],line,section)

            elif word0 == "END_GROUP":
                cur_group = "??"
                cur_group_units = "??"
                cur_group_dimstr = "??"

            else:
                #  profile declaration...

                if len(words) != 2:
                    swimgen_error(line,section,
                                  'syntax: <type> <array> expected.')

                #  separate variable name from dimensions
                var_decl = words[1].split('(')
                name = var_decl[0]

                #  may have to reassemble dimensions due to nested parentheses
                nldim = ""
                iwd = 1
                while iwd < len(var_decl):
                    nldim = nldim + var_decl[iwd]
                    iwd = iwd + 1
                    if iwd < len(var_decl):
                        nldim = nldim + "("

                if len(var_decl) > 1:
                    var_decl = [name,nldim]

                if len(var_decl) != 2:
                    swimgen_error(line,section,
                                  'array dimensions not found or garbled.')

                name = var_decl[0]
                cur_key = name.upper()
                check_key(cur_key,line,section)
                dupcheck(section,cur_key)

                #  separate variable type from qualifiers (if any)
                ddecl = words[0].split('|')

                #  variable type
                dtype = ddecl[0].upper()
                if dtype != "R":
                    swimgen_error(line,section,
                                  'all profiles must be of type "R".')

                ftype = standard_dtype(dtype,line,section)
                init = "0.0_rspec"

                if cur_component == "??":
                    swimgen_error(line,section,"component affiliation has not been specified.")

                if cur_group != "??":

                    #  group affiliation active: only standalone profiles
                    #  can be present (i.e. no indexing by species or any
                    #  list dimension)

                    if len(ddecl) > 3:
                        swimgen_error(line,section,
                                      'too many fields; expect at most data type, alias, and iterpolation method.')
                    elif len(ddecl) < 2:
                        swimgen_error(line,section,
                                      'too few fields; data type and interpolation method expected.')

                    alias = " "
                    cur_method = " "

                    for uitem in ddecl[1:]:
                        uispl = uitem.split('=')
                        if len(uispl) == 2:
                            ulhs = uispl[0].strip().upper()
                            if ulhs != "ALIAS":
                                swimgen_error(line,section,
                                              'keyword not expected: ' + ulhs)
                            alias = uispl[1].strip().upper()
                        elif len(uispl) == 1:
                            if cur_method != " ":
                                swimgen_error(line,section,
                                              'too many profile types or other syntax error.')
                            cur_method = uitem.upper()
                        else:
                            swimgen_error(line,section,
                                          'too many "=" signs; syntax error.')
                    if cur_method == " ":
                        swimgen_error(line,section,
                                      'interpolation method not found.')

                    [methkey,factor]=chk_method(cur_method,line,section)
                    if methkey == "HERMITE_EXPLICIT":
                        hermlist.append(cur_key)

                    [dlist,rank_coord,enum_codim] = aparse(var_decl[1],1,
                                                           maxrank_coord,0,
                                                           methkey,factor,
                                                           cur_component,
                                                           line,section)

                    #  set the alias...

                    if alias == " ":
                        if len(dlist) == rank_coord:
                            alias = name
                        else:
                            alias = name + "_"

                    chk_dimstr = gen_dimstr("ps%",dlist)

                    if cur_group_dimstr == "??":
                        cur_group_dimstr = chk_dimstr
                    else:
                        if chk_dimstr != cur_group_dimstr:
                            print("  group dimstr: ",cur_group_dimstr)
                            print("profile dimstr: ",chk_dimstr)
                            swimgen_error(line,section,
                                          "all elements within group must have same array dimensioning.")

                    if factor not in [" ","PS_DV","PS_DA"]:
                        fac_dlist = get_factor_dlist(factor,line,section)
                        fac_dimstr = gen_dimstr("ps%",fac_dlist)
                        if chk_dimstr != fac_dimstr:
                            print(" factor dimstr: ",fac_dimstr)
                            print("profile dimstr: ",chk_dimstr)
                            swimgen_error(line,section,
                                          "weight factor dimensioning and profile dimensioning are not consistent.")

                    # set up dictionary descriptor for profile...

                    idescr = {}
                    prof_descr(name,dtype,ftype,init,cur_group,cur_group_units,
                               cur_component,line,section,dlist,rank_coord,
                               methkey,factor,alias,
                               idescr)

                    if enum_codim > 0:
                        enum_codim = enum_codim - rank_coord
                        idescr['enum_codim'] = enum_codim


                    all_groups[cur_group].append(name.upper())

                else:
                    # profile array designator (no group affiliation)
                    #   species or list dimensioning may be present.

                    if len(ddecl) > 4:
                        swimgen_error(line,section,
                                      'too many fields; expect at most data type, units, alias, and iterpolation method.')
                    elif len(ddecl) < 3:
                        swimgen_error(line,section,
                                      'too few fields; units, data type and interpolation method expected.')

                    alias = " "
                    cur_method = " "
                    units = "??"
                    for uitem in ddecl[1:]:
                        uispl = uitem.split('=')
                        if len(uispl) == 2:
                            ulhs = uispl[0].upper().strip()
                            if ulhs == "ALIAS":
                                alias = uispl[1].strip().upper()

                            elif ulhs == "UNITS":
                                units = uparse(uitem,line,section)

                            else:
                                swimgen_error(line,section,
                                              'keyword not recognized: ' + ulhs)

                        elif len(uispl) == 1:
                            if cur_method != " ":
                                swimgen_error(line,section,
                                              'too many profile types or other syntax error.')
                            cur_method = uitem.upper()
                        else:
                            swimgen_error(line,section,
                                          'too many "=" signs; syntax error.')

                    if cur_method == " ":
                        swimgen_error(line,section,
                                      'interpolation method not found.')
                    if units == "??":
                        swimgen_error(line,section,
                                      'profile units not found.')

                    [methkey,factor]=chk_method(cur_method,line,section)
                    if methkey == "HERMITE_EXPLICIT":
                        hermlist.append(cur_key)

                    [dlist,rank_coord,enum_codim] = aparse(var_decl[1],1,
                                                           maxrank_coord,
                                                           maxrank_list,
                                                           methkey,factor,
                                                           cur_component,
                                                           line,section)

                    #  set the alias...

                    if alias == " ":
                        if len(dlist) == rank_coord:
                            alias = name
                        else:
                            alias = name + "_"

                    if factor not in [" ","PS_DV","PS_DA"]:
                        chk_dimstr = gen_dimstr("ps%",dlist)
                        fac_dlist = get_factor_dlist(factor,line,section)
                        fac_dimstr = gen_dimstr("ps%",fac_dlist)
                        if chk_dimstr != fac_dimstr:
                            print(" factor dimstr: ",fac_dimstr)
                            print("profile dimstr: ",chk_dimstr)
                            swimgen_error(line,section,
                                          "weight factor dimensioning and profile dimensioning are not consistent.")

                    # set up dictionary descriptor for profile...

                    idescr = {}
                    prof_descr(name,dtype,ftype,init,cur_group,units,
                               cur_component,line,section,dlist,rank_coord,
                               methkey,factor,alias,
                               idescr)

                    if enum_codim > 0:
                        enum_codim = enum_codim - rank_coord
                        idescr['enum_codim'] = enum_codim


    if cur_key != "??":
        if len(cmts) == 0:
            swimgen_error(line,section,
                          "comments not found for profile: " + cur_key)
        idescr['cmts'] = cmts
        all_items[cur_key] = idescr

    #  for items of type "Hermite Explicit", generate derivative arrays
    #  these are cloned from the original array; key, name, & methkey are
    #  modified...

    for key in hermlist:
        descr = all_items[key]
        dlist = descr['dlist']
        rank_coord = descr['rank_coord']
        coordlist = get_coords(dlist[0:rank_coord])
        loc_deriv_list = []
        mkderivs(key,coordlist,[],loc_deriv_list)
        descr['loc_deriv_list'] = loc_deriv_list

#--------------------------------------------------------------------------
def splist_update(item_flag,key,section,action):

    # append item key to special list

    global species_list_items
    global species_list_attrib
    global item_list_items
    global grid_items

    global mdescr
    global sconfig
    global simcon
    global state_data_list

    if action == "add":
        if item_flag == "SPECIES_LIST":
            species_list_items.append(key)
            species_list_attrib.append(key)
        elif item_flag == "LIST":
            item_list_items.append(key)
        elif item_flag == "GRID":
            grid_items.append(key)
        else:
            if section == "MACHINE_DESCRIPTION":
                mdescr.append(key)
            elif section == "SHOT_CONFIGURATION":
                sconfig.append(key)
            elif section == "SIMULATION_INIT":
                simcon.append(key)
            elif section == "STATE_DATA":
                state_data_list.append(key)
            else:
                print(" item_flag: ",item_flag)
                print(" section:   ",section)
                print("   ?splist_update: unknown disposition.")
                raise splist_update_error
    elif action == "remove":
        if item_flag == "SPECIES_LIST":
            species_list_items.remove(key)
            species_list_attrib.remove(key)
        elif item_flag == "LIST":
            item_list_items.remove(key)
        elif item_flag == "GRID":
            grid_items.remove(key)
        else:
            if section == "MACHINE_DESCRIPTION":
                mdescr.remove(key)
            elif section == "SHOT_CONFIGURATION":
                sconfig.remove(key)
            elif section == "SIMULATION_INIT":
                simcon.remove(key)
            elif section == "STATE_DATA":
                state_data_list.remove(key)
            else:
                print(" item_flag: ",item_flag)
                print(" section:   ",section)
                print("   ?splist_update: unknown disposition.")
                raise splist_update_error
    else:
        print(" ?splist_update: unrecognized action argument: ",action)
        raise splist_update_error

#--------------------------------------------------------------------------
def get_coords(dlist):

    # find the coordinates corresponding to passed dimensioning information
    # note that "-1" may need to be stripped from dimension descriptor

    coordlist = []

    for dpair in dlist:
        # use upper dimension
        # strip off trailing "-1" if necessary
        dchk = dpair[1]
        if dchk[-2:] == "-1":
            dchk = dchk[:-2]

        dkey = dchk.upper()
        ddescr = all_items[dkey]

        dimtype = ddescr['dimtype']
        if dimtype not in all_coords:
            print(" ?? get_coords internal error: not a grid dimension: ",dkey)
            raise get_coords_error

        coordlist.append(dimtype)

    return coordlist

#--------------------------------------------------------------------------
def mkderivs(key,coordlist,startlist,loc_deriv_list):

    global all_profiles
    global all_items
    global deriv_list

    #  *recursive*
    #  generate definitions of derivative profiles
    #    all derivatives of the profile indicated by "key"
    #    along each coordinate... higher derivatives have non-empty startlist

    #  derivatives from:
    descr = all_items[key]
    name = descr['name']
    units = descr['units']

    klist = list(descr.keys())

    #  derivatives of this order to be defined:
    order = len(startlist) + 1

    #  derivatives already taken:
    str = ""
    ustr = ""
    for coord in startlist:
        str = str + "d" + coord
        ustr = ustr + "/" + all_coords[coord]

    #  "numerator" of derivativce expression
    if order == 1:
        dname_num = "d" + name + "_"
    else:
        dname_num = "d" + repr(order) + name + "_"

    #  a new derivative for each coordinate in the list
    for coord in coordlist:
        dname = dname_num + "d" + coord + str
        dunits = units + "/" + all_coords[coord] + ustr
        # print dname
        # print dunits

        ddescr = {}
        for kkey in klist:
            ddescr[kkey] = descr[kkey]

        dkey = dname.upper()
        ddescr['methkey'] = "Hermite_derivative"
        ddescr['name'] = dname
        ddescr['units'] = dunits
        all_items[dkey] = ddescr
        all_profiles.append(dkey)
        deriv_list.append(dkey)
        loc_deriv_list.append(dkey)

    #  max order = len(coordlist) + len(startlist)
    #  if haven't reached the max, get some more...

    ilen = len(coordlist) - 1
    if ilen > 0:
        icount = 0
        for coord in coordlist[1:]:
            icount = icount + 1
            new_start = [coord] + startlist
            mkderivs(key,coordlist[0:icount],new_start,loc_deriv_list)

def chk_method(interp_method,line,section):

    #  expect "step" or "step*name" or "step*name(subexpr)"
    #    or "step*dV" or "step*dA"
    #  or "pclin" or "hermite" or "spline"

    mpcs = interp_method.split('*')
    if len(mpcs) == 1:
        methkey = interp_method.upper()
        factor = " "
    else:
        methkey = mpcs[0].upper()
        if methkey != "STEP":
            swimgen_error(line,section,
                          '"*" weighting available for STEP profiles only.')
        factor = mpcs[1]
        facu = factor.upper()

        # check for dVol or dArea factor...
        if facu == "DA" or facu == "DV":
            factor = "PS_" + facu
            return [methkey,factor]

        # profile normalization factor detected (apparently)...

        mpc2 = factor.split('(')
        mname = mpc2[0].upper()
        if mname not in all_items:
            swimgen_error(line,section,
                          '"*" weighting must refer to a profile: ' + mname +
                          ' not recognized.')

        ref_descr = all_items[mname]
        ref_section = ref_descr['section']

        if ref_section != "STATE_PROFILES":
            swimgen_error(line,section,
                          '"*" weighting must refer to a profile: ' + mname +
                          ': not from profile section.')

    return [methkey,factor]

def aparse(adims,minrank_coord,maxrank_coord,maxrank_list,methkey,factor,
           cur_component,line,section):

    #  parse an array declaration fragement <[ldim:]udim>[,<[ldim:]udim>,...])
    #     (separated from <name>(<dims...>) by a split with ")").

    #  limit coordinate rank to maxrank_coord; limit list rank to
    #  maxrank_list

    #  ALSO: once a list dimension "S" or "L" dimension is detected, all
    #  subsequent dimensions must also be of type "S" or "L"; i.e.:
    #  declare all coordinate dims first then all list dims.

    #----------------------------
    #  this routine enforces the coordinate then list dimension ordering:

    [enum_code,enum_codim,ienum,dlist] = get_dimlist(adims,cur_component,line,
                                                     section)

    #  count the coordinate dims; if methkey = "STEP" check for "-1"...

    irank_coord = 0
    irank_list = 0

    for dimpair in dlist:
        dimu = dimpair[1]
        if dimu[-2:] == "-1":
            im1="TRUE"
            dimu=dimu[:-2]
        else:
            im1="FALSE"

        # rely on checks in get_dimlist, to be sure that dimu is in fact
        # an array dimension

        ddescr = all_items[dimu.upper()]
        dimtype = ddescr['dimtype']
        if dimtype in all_coords:
            irank_coord = irank_coord + 1
            if methkey == "STEP":
                if im1 == "FALSE":
                    swimgen_error(line,section,
                                  ' "~" designator required in coord dimensions for STEP profiles.')
                if factor != " ":
                    if irank_coord > 1 or dimtype != "RHO":
                        print(" irank_coord = ",irank_coord)
                        print("    dimtype = ",dimtype)
                        swimgen_error(line,section,
                                      ' STEP*<factor> profiles must be vs. RHO only.')
            else:
                if im1 == "TRUE":
                    swimgen_error(line,section,
                                  ' "~" designator must not be present for non-STEP profiles.')

        else:
            irank_list = irank_list + 1

    ierr = 0
    if irank_list > maxrank_list:
        ierr = ierr + 1
        print(" ********* ")
        print("Number of list dimensions exceeds maximum (0 if in a group): ",maxrank_list)
    if irank_coord > maxrank_coord:
        ierr = ierr + 1
        print("Number of coord dimensions exceeds maximum: ",maxrank_coord)
    if irank_coord < minrank_coord:
        ierr = ierr + 1
        print("At least one coordinate dimensions required in profile declaration.")

    if ierr > 0:
        if maxrank_list == 0:
            print("(no list dimensions allowed in group members)")
        swimgen_error(line,section,'error in array declaration.')

    return [dlist,irank_coord,enum_codim]

def standard_dtype(dtype,line,section):

    # map "R" -> REAL(KIND=rspec), etc.
    # error if dtype is unrecognized

    global char_lens

    digits = '0123456789'

    if dtype == "R":
        ftype = 'REAL(KIND=rspec)'

    elif dtype == "G" and section == "STATE_PROFILES":
        ftype = 'REAL(KIND=rspec)'

    elif dtype == "I":
        ftype = 'INTEGER'

    elif dtype == "F":
        ftype = 'CHARACTER*256'

    elif dtype == "N":
        ftype = 'CHARACTER*32'

    elif dtype[0:2] == "C*":
        if len(dtype) == 2:
            swimgen_error(line,section,'bare "C*" is not a valid type code.')

        for char in dtype[2:]:
            if char not in digits:
                swimgen_error(line,section,
                              'string after "C*" is not an integer.')

        iclen = int(dtype[2:])

        if iclen > 256:
            swimgen_error(line,section,'string length too long (max: 256)')

        if iclen not in char_lens:
            char_lens.append(iclen)

        ftype = 'CHARACTER*' + dtype[2:]

    else:
        if dtype == "G":
            print("G data type restricted to STATE_PROFILES section.")
        swimgen_error(line,section,
                      'data type code not recognized (expected "R", "I", "F", etc.)')

    return ftype


def dupcheck(section,key):

    # check for duplicate names in all_items

    if key in all_items or key in all_groups:
        if key[0:4] == 'MAX_':
            dkey = key[4:]
            if dkey in all_items:
                print(' ...the name ' + key + ' may have been derived from: ' + dkey)
        swimgen_error(" ",section,"duplicate name: " + key)

    if key[0:3] == 'PS_':
        print(" variable name (key): " + key)
        swimgen_error(" ",section,'prefix "PS_" reserved for code generator, not allowed for state variables.')

    if key in ["DV","DA"]:
        swimgen_error(" ",section,'the name"' + key + '" is reserved by the state generator software.')


def check_key(key,line,section):

    # key is name in uppercase; it must be alphanumeric and the leading
    # character must be alphabetic

    alphab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    digpls = '0123456789_'

    if key[0] not in alphab:
        swimgen_error(line,section,
                      'first character of name "' + key + '" must be alphabetic')

    for char in key[1:]:
        if char not in alphab:
            if char not in digpls:
                swimgen_error(line,section,
                              'alphanumeric characters and "_" only in names:'+
                              ' "' + key + '"')

    if section != "CONSTANTS":
        if key[0:3] == "PS_":
            swimgen_error(line,section,
                          'The prefix "PS_" is reserved for constants.')

    if key in reserved_names:
        print("reserved_names: ",reserved_names)
        swimgen_error(line,section,
                      'The name "' + key + '" is reserved and cannot be used.')

def uparse(str,line,section):

    # parse "units=<units>" fragment

    udef = str.split('=')
    if len(udef) != 2:
        swimgen_error(line,section,
                      'expected |units=<units> declaration (lhs=rhs).')
    uword = udef[0].upper()
    if uword != "UNITS":
        swimgen_error(line,section,
                      'expected |units=<units> declaration (lhs=units).')

    cur_units = udef[1]

    return cur_units

def slist_array_descr(name,sname,iname,
                      dtype,ftype,init,cur_group,units,component,line,section,
                      dlist):

    # add a data array derived from a species list specification

    global all_items
    global species_list_items
    global species_list_attrib

    kdescr = {}

    array_descr(sname,dtype,ftype,init,cur_group,units,component,line,section,
                dlist, kdescr)

    kdescr['slist'] = name

    if iname == "type":
        cmt = name.upper() + ' specie types'
    elif iname == "q":
        cmt = name.upper() + ' specie charge'
    elif iname == "qatom":
        cmt = name.upper() + ' atomic number'
    elif iname == "m":
        cmt = name.upper() + ' specie mass'
    else:
        raise slist_array_descr_error

    cmts = []
    cmts.append(cmt)

    kdescr['cmts'] = cmts

    tmp_key = sname.upper()

    check_key(tmp_key," ",section)
    dupcheck(section,tmp_key)

    all_items[tmp_key] = kdescr
    species_list_items.append(tmp_key)
    species_list_attrib.append(tmp_key)

def prof_descr(name,dtype,ftype,init,cur_group,units,component,line,section,
                dlist,rank_coord,methkey,factor,alias, idescr):

    global all_profiles
    global profile_list

    # set up a profile array description
    # = array description + [addl arguments btw dlist and idescr].

    array_descr(name,dtype,ftype,init,cur_group,units,component,line,section,
                dlist,idescr)

    idescr['rank_coord'] = rank_coord
    idescr['methkey'] = methkey
    idescr['factor'] = factor
    idescr['alias'] = alias

    prof_key = name.upper()
    all_profiles.append(prof_key)

    if methkey != "Hermite_derivative":
        profile_list.append(prof_key)

def array_descr(name,dtype,ftype,init,cur_group,units,component,line,section,
                dlist,idescr):

    # set up a array description
    # = scalar description plus array dimensions...

    scalar_descr(name,dtype,ftype,init,cur_group,units,component,line,section,
                 idescr)

    idescr['dlist'] = dlist

    # record size increments relative to dimension variable that are seen
    # among all array declarations (each will have to be a separate dimension
    # in the NetCDF file representation of the state).

    enum_rank = 0
    icount = 0

    for dpair in dlist:
        icount = icount + 1

        dlower = dpair[0]
        dupper = dpair[1]

        if dupper[-2:] == "-1":
            incr = -1
            dimname = dupper[:-2]
        else:
            incr = 0
            dimname = dupper

        if dlower != "I":
            dkey = dimname.upper()
            ddescr = all_items[dkey]
            ilist = ddescr['ilist']
        else:
            ilist = [0]

        if dlower == "0":
            incr = incr + 1
        elif dlower == "-1":
            incr = incr + 2
        elif dlower == "I":
            enum_rank = enum_rank + 1

        if incr not in ilist:
            ilist.append(incr)
            ilist.sort()
            ddescr['ilist'] = ilist

    idescr['enum_rank'] = enum_rank
    if enum_rank > 0:
        if dtype != "R" and dtype != "I":
            swimgen_error(line,section,
                          "only R and I arrays can have an enumeration dimension.")
        if enum_rank > 1:
            swimgen_error(line,section,
                          "limit of ONE enumeration dimension per item still in force.")

def gen_dimstr(prefix,dlist):

    # generate dimensioning string from dlist; apply prefix to each symbol

    dimstr = "("

    ictr = 0
    for dpair in dlist:
        ictr = ictr + 1
        if ictr == len(dlist):
            finchar = ")"
        else:
            finchar = ","
        if dpair[0] == "1" or dpair[0] == "I":
            dkey = dpair[1].upper()
            dimlabel = 'OK'
            if dkey[-2:] != "-1":
                ddescr = all_items[dkey]
                dimlabel = ddescr['dimlabel']
            if dimlabel == "enum_vec":
                dimstr = dimstr + 'maxval(' + prefix + dpair[1] + ')' + finchar
            else:
                dimstr = dimstr + prefix + dpair[1] + finchar
        else:
            dimstr = dimstr + dpair[0] + ":" + prefix + dpair[1] + finchar

    return dimstr

def scalar_descr(name,dtype,ftype,init,cur_group,units,component,line,section,
                 idescr):

    # set up a scalar description

    maxlen = 19

    if len(name) > maxlen:
        print("scalar_descr: name length .gt. ",maxlen,": ",name)
        raise name_length_error

    idescr['name'] = name      # name
    idescr['dtype'] = dtype    # type code "R", "I", etc.
    idescr['ftype'] = ftype    # fortran type declaration
    idescr['init'] = init      # initial value string
    if cur_group != "??":
        idescr['group'] = cur_group     # group affiliation (if any)
    if dtype == "R":
        idescr['units'] = units         # physical units (R type only)

    idescr['component'] = component     # component
    idescr['spec'] = line
    idescr['section'] = section.upper()

def dim_define(var_decl,line,section):

    # dimension definition -- var_decl[0] an array name; var_decl[1] the
    # single dimension ending in ")"

    if len(var_decl) != 2:
        swimgen_error(line,section,
                      'syntax error in dimension declaration')

    dimnam0 = var_decl[1]
    dimnam1 = dimnam0[:-1]

    dimchk = dimnam1.split(",")
    if len(dimchk) > 1:
        swimgen_error(line,section,
                      'multi-dimension syntax not possible in dimension declaration')

    dimclist = dimnam1.split(":")
    if len(dimclist) == 1:
        dlowval = "1"
        dimname = dimclist[0]

    elif len(dimclist) == 2:
        dlowval = dimclist[0]
        dimname = dimclist[1]

        chk_dlowval(dlowval,line,section)

    else:
        swimgen_error(line,section,
                      'syntax error, too many colons ":"')

    return [dlowval,dimname]

def get_dimlist(tparen,cur_component,line,section):

    # given a dimensioning string of form "<dpair>[,<dpair>...])"
    # with trailing right parenthesis and each dimension
    #   <dpair>
    # of form either a single dimension or dim1:dim2, return
    # the list of dimension pairs.  If form is a single dimension
    # assume dim1 = 1

    # examples:
    #    "nbeam)" -> [["1","nbeam"]]
    #    "~nrho_gas,0:nspec_th)" -> [["1","nrho_gas-1"],["0","nspec_th"]]

    # error checks:
    #   (a) LOWER dimensions if present: use chk_dlowval; upper dimension
    #       must be of type "S" or "L".
    #   (b) UPPER dimensions: must be recognized dimension
    #        --translate "~<dim>" to "<dim>-1": must be a grid dimension.
    #   (c) multiple dimensions:
    #       either all or "S" & "L" dimensions
    #       or 1 or more "G" dimensions followed by "S" & "L" dimensions
    #   (d) if an "I" dimension: there can only be one dimension total;
    #       no lower limits or "~" designator.

    # enumeration dimension: a single dimension for "enumeration" is allowed;
    # it must be first or the first following grid dimensions.  The
    # enumeration dimension can be an integer or an integer vector which
    # shares a list dimensions with the list dimensions of the current
    # array.

    if tparen[-1] != ")":
        swimgen_error(line,section,
                      'array declaration error, trailing ")" not found.')

    dimstr = tparen[:-1]
    dsplit = dimstr.split(",")

    inumd = len(dsplit)

    dlist = []

    enum_code = "NONE"
    ienum = -1
    enum_codim = 0

    id_count = 0
    have_sl = "FALSE"

    dck = ""
    dck_match = "FALSE"

    for dimpair in dsplit:
        id_count = id_count + 1
        dpair = dimpair.split(":")
        im1 = "FALSE"
        ivec = "FALSE"
        if len(dpair) == 1:
            have_lower = "FALSE"
            dlower = "1"
            dupper = dpair[0]

            # check for "~" syntax
            if dupper[0] == "~":
                dtmp = dupper[1:]
                dkey = dtmp.upper()
                im1 = "TRUE"
                dupper = dtmp + "-1"
            else:
                if dupper[-2:] == "()":
                    # variable length enumeration...
                    # allocated array dimension is max_<dimname>
                    dkey = dupper[:-2].upper()
                    dupper = dupper[:-2]
                    ivec = "TRUE"
                else:
                    dkey = dupper.upper()

        elif len(dpair) == 2:
            #  lower dimension present: rule (a)...

            have_lower = "TRUE"

            dlower = dpair[0]
            dupper = dpair[1]

            chk_dlowval(dlower,line,section)

            dkey = dupper.upper()

        else:
            swimgen_error(line,section,
                          'syntax error in dimension spec: "' + dimpair + '".')


        if dupper.upper() not in species_list_items:
            ddesc1 = all_items[dkey]
            cctest = ddesc1['component']
            if cctest != cur_component:
                print(" ... ",dupper," belongs to component ",cctest)
                print("     but component of this declaration is: ",cur_component)
                print("     dkey: ",dkey)
                swimgen_error(line,section,"component membership mismatch")

        if dkey not in all_dims:
            # enumeration dimension possible: integer scalar or
            # integer 1d vector, must be 1st dim; overal rank must be 2 or less
            # there can be no "~" or
            test_key = "MAX_" + dkey
            if ivec == "TRUE" and test_key in all_dims:
                if have_sl == "TRUE" or enum_code != "NONE":
                    if have_sl == "TRUE":
                        print("...prior list dimension detected.")
                    if enum_code != "NONE":
                        print("...multiple enumeration dimensions detected.")
                    swimgen_error(line,section,
                                  'enumeration dimension (' + dupper + ') must be first or follow coordinate dimensions.')
                if im1 == "TRUE" or have_lower == "TRUE":
                    print(' dimension specification: "' + dimpair + '".')
                    swimgen_error(line,section,
                                  'enumeration dimension cannot have lower limit or "~" qualifier.')
                enum_code = "I"
                ienum = id_count - 1
                dlower = "I"
                dupper = "max_" + dupper

                idescr = all_items[dkey]
                if 'dlist' not in idescr:
                    swimgen_error(line,section,
                                  'unexpected "()" designation for scalar dimension: ' + dkey)
                else:
                    ck_dlist = idescr['dlist']
                    if len(ck_dlist) != 1:
                        swimgen_error(line,section,
                                      'variable length "()" enumeration dimension needs to be a 1d integer array')
                    dck = ck_dlist[0][1].lower()
                    dck_key = dkey
            else:
                test_key in "MAX_" + dkey
                if test_key in all_dims:
                    print(' ...(use "' + dkey.lower() + '()" for variable length enumeration dimension).')
                swimgen_error(line,section,
                              '"' + dimpair + '": array dimension symbol not recognized.')
        else:
            ddescr = all_items[dkey]
            dimtype = ddescr['dimtype']

            if dimtype == "I":
                if have_sl == "TRUE" or enum_code != "NONE":
                    if have_sl == "TRUE":
                        print("...prior list dimension detected.")
                    if enum_code != "NONE":
                        print("...multiple enumeration dimensions detected.")
                    swimgen_error(line,section,
                                  'enumeration dimension (' + dupper + ') must be first or follow coordinate dimensions.')
                if im1 == "TRUE" or have_lower == "TRUE":
                    print(' dimension specification: "' + dimpair + '".')
                    print(' "I" dimension must be used singly with no lower limit or "~" qualifier.')
                    swimgen_error(line,section,
                                  '"I" dimension usage error.')
                idescr = all_items[dkey]
                if ivec == "TRUE":
                    if 'dlist' not in idescr:
                        swimgen_error(line,section,
                                      'variable length "()" enuneration dimension needs to be a 1d integer array')
                    else:
                        ck_dlist = idescr['dlist']
                        dck = ck_dlist[0][1].lower()
                        dck_key = dkey
                if ivec == "FALSE" and 'dlist' in idescr:
                    swimgen_error(line,section,
                                          'use "()" to denote variable length enumeration dimension.')

                enum_code = "I"
                ienum = id_count - 1
                dlower = "I"

            elif dimtype in all_coords:
                if have_sl == "TRUE":
                    print(' grid dimension must not follow list dimension.')
                if have_lower == "TRUE":
                    print(' dimension lower limit expression not valid with grid dimension.')
                if have_sl == "TRUE" or have_lower == "TRUE":
                    swimgen_error(line,section,'error with grid dimension: "' +
                                  dimpair + '".')

            else:
                have_sl = "TRUE"
                if im1 == "TRUE":
                    swimgen_error(line,section,
                                  '"~" designator not valid for list dimension: "' + dimpair + '".')
                ilower = int(dlower)
                jlower = int(ddescr['dim_lval'])
                if ilower != jlower:
                    print('\n expected dimensioning: "' + ddescr['dim_lval'] + ':' + dkey.lower() + '"')
                    swimgen_error(line,section,
                                  'lower limit inconsistency.')
                if dck != "":
                    if dupper == dck:
                        dck_match = "TRUE"
                        enum_codim = id_count

        dlist.append([dlower,dupper.lower()])

    if dck != "":
        if dck_match != "TRUE":
            print(" dimensioning of " + dck_key + ": " + dck)
            print(" unmatched by subsequent dimensions of declaration:")
            swimgen_error(line,section, 'dimension matching test failed.')

    return [enum_code,enum_codim,ienum,dlist]

def chk_dlowval(dlowval,line,section):

    # check lower value of <n>:<name> style of dimension declaration
    # at present only 0 or -1 are accepted!

    if dlowval != "0" and dlowval != "-1":
        swimgen_error(line,section,
                      'explicit dimension lower limit must be either "0" or "-1"')

def get_factor_dlist(factor,line,section):

    #  get dimension list for indicated factor
    #  if of form "name" this is the named profile's dlist
    #  if of form "name(:,expr)" it is the indicated subset of the
    #        profile's dlist.

    faclist = factor.split("(")

    facroot = faclist[0]
    pkey = facroot.upper()
    if pkey not in all_items:
        swimgen_error(line,section,
                      'STEP weight factor profile name unrecognized.')
    pdescr = all_items[pkey]
    if pdescr['section'] != "STATE_PROFILES":
        swimgen_error(line,section,
                      'STEP weight factor is not a previously defined profile.')
    if len(faclist) == 1:
        dlist = pdescr['dlist']

    elif len(faclist) > 2:
        swimgen_error(line,section,
                      ' syntax error in weight factor expression: ' + factor)

    else:
        dlist_orig = pdescr['dlist']
        rank_coord = pdescr['rank_coord']
        rank_list = len(dlist_orig) - rank_coord
        dlist = dlist_orig[0:rank_coord]

        # check the list-element specification
        expr = faclist[1][:-1]
        explist = expr.split(",")
        if len(explist) != rank_list + 1 or explist[0] != ":":
            print('factor address expression should be of form')
            print('  (:,<ps_const1>,<ps_const2>,...)')
            print('  with a "ps_" integer parameter for each list dimension.')
            swimgen_error(line,section,
                          ' syntax error in weight factor expression: ' + factor)
        for ps_param in explist[1:]:
            ps_test = ps_param.upper()
            if ps_test not in all_constants:
                print("unrecognized constant parameter name: " + ps_param)
                swimgen_error(line,section,
                              ' syntax error in weight factor expression: ' + factor)

    return dlist

def swimgen_error(line,section,msg):

    print(" ")
    print(" ?plasma_state_gen: error processing text line: ")
    print("     " + line)
    print("  in section: " + section)
    print("  message is: " + msg)
    print(" ")

    raise plasma_state_gen_error_exit

def elem_category(idescr):

    # categorize an item with a simple character string, lowercase
    # by convention

    kcat = "????"

    if 'dlist' not in idescr:
        # scalar or array dimension
        if 'dimtype' not in idescr:
            kcat = "scalar"
        else:
            dimtype = idescr['dimtype']
            if dimtype == "I":
                kcat = "e_dim"
            else:
                kcat = "dim"
    else:
        dlist = idescr['dlist']

        if 'rank_coord' in idescr:
            rank_coord = idescr['rank_coord']
            if rank_coord < len(dlist):
                kcat = "prof_array"
            else:
                kcat = "prof"

        else:
            enum_rank = idescr['enum_rank']
            if enum_rank == 0:
                kcat = "array"
            else:
                if enum_rank < len(dlist):
                    kcat = "enum_array"
                else:
                    kcat = "enum"

    if kcat == "????":
        print(idescr)
        print(" ")
        print(" ? elem_category: categorization error. ")
        print(" ")
        raise elem_category_error

    return kcat

def get_enum_dim(idescr,indx):

    # get expression for enumeration dimension

    enum_rank = idescr['enum_rank']
    if enum_rank == 0:
        print(idescr)
        print(" ")
        print(" ? get_enum_dim: python3 script error: not an enumeration.")
        print(" ")
        raise get_enum_dim_error

    dlist = idescr['dlist']

    for pair in dlist:
        if pair[0] == "I":
            dim = pair[1]

    if indx == "":
        return dim

    return dim + "(" + indx + ")"

def adjust_maxrank(maxrank,dlist,ienum):

    # adjust maxrank: add 1 if 1st dim of dlist is an enumeration dimension

    imax = maxrank

    dkey = dlist[ienum][1].upper()
    idescr = all_items[dkey]
    dimlabel = idescr['dimlabel']
    if dimlabel == "enum" or dimlabel == "enum_vec" or dimlabel == "enum_max":
        imax = imax + 1

    return imax
