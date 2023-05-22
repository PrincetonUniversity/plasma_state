#!/usr/bin/env python3

#
# This file will write the f77 compliant interfaces to set/get
# plasma_state members
#

import string

import os

#--------------------------------------------------------------------------
# global items...

from plasma_state_global import *

#--------------------------------------------------------------------------
# utility routines and error handling...

from plasma_state_sub import *

import re
import time

def breakComment(txt, maxlen=72, comment='!'):
        """ Break comment to fit max line length """

        if len(txt) <= maxlen: return txt

        spacePositions = []
        outTxt = ''
        startPos = 0
        endPos   = 0
        lin = 0
        while endPos >= 0:
                com = ''
                if startPos + maxlen >= len(txt):
                        outTxt += txt[startPos:]
                        endPos = -1
                else:
                        endPos = txt.rfind(' ', startPos, startPos + maxlen)
                        outTxt += txt[startPos:endPos] + '\n' + comment
                startPos = endPos + 1
                lin += 1
        return outTxt


def write_setget_body(f, prms):
        """
        Write the body of a set/get method
        """

        body = """
subroutine %(routine name)s%(arg list)s
    %(comment)s
    use plasma_state_mod, only : plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*)  ! object handle
    %(data dims decl)s ! list of dimensions (if any)
    %(data type)s%(data inout)s  :: data%(data rank)s ! to read to/write from
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('%(routine name)s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr %% ps_ptr
    ! set
    %(left expr)sss %% %(member name)s%(right expr)s
end subroutine %(routine name)s
""" % prms

        # now write
        print(body, file=f)

def write_getNumDims_body(f, prms):
        """
        Write the body of a getNumDims method
        """

        body = """
subroutine %(routine name)s(iobj, ndims, ierr)
    %(comment)s
    use plasma_state_mod, only :  plasma_state_container, plasma_state, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: ndims  ! number of dimensions
    integer, intent(out) :: ierr   ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    ndims = %(data numDims)d
end subroutine %(routine name)s
""" % prms

        # now write
        print(body, file=f)


def write_getCharacterSize_body(f, prms):
        """
        Write the body of a getSizeCharacter method
        """

        body = """
subroutine %(routine name)s(iobj, size, ierr)
    %(comment)s
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(out) :: size    ! size of character string
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    call ccps_argcheck('%(routine name)s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr %% ps_ptr
    ! note that the following will return the character length size regardless of
    ! the number of array elements (if %(member name)s is an array)
    size = len(ss %% %(member name)s)
end subroutine %(routine name)s
""" % prms

        # now write
        print(body, file=f)

def write_getDims_body(f, prms):
        """
        Write the body of a getDims method
        """

        body = """
subroutine %(routine name)s(iobj, ndims, dims, ierr)
    %(comment)s
    use plasma_state_mod, only :  plasma_state_container, plasma_state, &
                                & load_ps_container, rspec
    implicit NONE
    integer, intent(in) :: iobj(*) ! object handle
    integer, intent(in) :: ndims   ! number of dimensions
    integer, intent(out) :: dims(ndims) ! sizes
    integer, intent(out) :: ierr ! error code (0=OK)
    ! local
    type (plasma_state_container) :: ptr
    type (plasma_state), pointer  :: ss

    ierr = 0
    if (ndims /= %(data numDims)d) ierr = 1
    call ccps_argcheck('%(routine name)s', iobj, ierr)
    if(ierr.ne.0) return
    ! proceed...
    call load_ps_container(iobj,ptr)
    ss => ptr %% ps_ptr
    dims = 0
    if (allocated( ss %% %(member name)s ) ) then
        ! can only call 'shape' on allocated/associated arrays
        dims = shape(ss %% %(member name)s)
    endif
end subroutine %(routine name)s
""" % prms

        # now write
        print(body, file=f)


def write_setCPrototype(f, prms):
        """
        Write C set prototype
        """

        body = """
/**
 * Set %(variable description)s
 * @param iobj reference to opaque plasma state object """ % prms
        dims = re.sub(r' ','', prms['data dims'])
        if dims:
                counter = 0
                for dim in dims.split(','):
                        body += """
 * @param %s data dimension along axis %d """ % (dim, counter)
                        counter += 1
        body += """
 * @param data data to set
 * @param ierr error code (0=OK)
 */
#define %(C routine name)s F77NAME(%(routine name)s)
void %(C routine name)s(int* iobj, """ % prms
        if dims:
                for dim in dims.split(','):
                        body += "const int* %s, " % dim
        if prms['C type'] != 'char':
                body += "const %(C type)s* data, int* ierr);" % prms
        else:
                # add string length as last (invisible from fortran) argument
                body += "const %(C type)s* data, int* ierr, size_t data_len);" % prms

        print(body, file=f)

def write_setCxx(f, prms):
        """
        Associate C set method
        """
        body = """    _set%(C++ type descr)s["%(lower case member name)s"] = %(C routine name)s;"""%prms
        print(body, file=f)

def write_getCxx(f, prms):
        """
        Associate C get method
        """
        body = """    _get%(C++ type descr)s["%(lower case member name)s"] = %(C routine name)s;"""%prms
        print(body, file=f)

def write_getNumDimsCxx(f, prms):
        body = """    _getNumDims["%(lower case member name)s"] = %(C routine name)s;""" % prms
        print(body, file=f)

def write_getDimsCxx(f, prms):
        body = """    _getDims["%(lower case member name)s"] = %(C routine name)s;""" % prms
        print(body, file=f)


def write_getCharacterSizeCxx(f, prms):
        body = """    _getCharacterSize["%(lower case member name)s"] = %(C routine name)s;""" % prms
        print(body, file=f)

def write_getCPrototype(f, prms):
        """
        Write C get prototype
        """

        body = """
/**
 * Get %(variable description)s
 * @param iobj reference to opaque plasma state object """ % prms
        dims = re.sub(r' ','', prms['data dims'])
        if dims:
                counter = 0
                for dim in dims.split(','):
                        body += """
 * @param %s data dimension along axis %d """ % (dim, counter)
                        counter += 1
        body += """
 * @param data data to set
 * @param ierr error code (0=OK)
 */
#define %(C routine name)s F77NAME(%(routine name)s)
void %(C routine name)s(int* iobj, """ % prms
        if dims:
                counter = 0
                for dim in dims.split(','):
                        body += "const int* %s, " % dim
        if prms['C type'] != 'char':
                body += "%(C type)s* data, int* ierr);" % prms
        else:
                # add string length as last (invisible from fortran) argument
                body += "%(C type)s* data, int* ierr, size_t data_len);" % prms

        print(body, file=f)

def write_getNumDimsCPrototype(f, prms):
        """
        Write C prototype for returning the number of dimensions
        """

        body = """
/**
 * Get number of dimension of %(variable description)s
 * @param iobj reference to opaque plasma state object
 * @param ndims number of dimensions
 * @param ierr error code (0=OK)
 */
#define %(C routine name)s F77NAME(%(routine name)s)
void %(C routine name)s(int* iobj, int* ndims, int* ierr); """ % prms
        print(body, file=f)

def write_getDimsCPrototype(f, prms):
        """
        Write C prototype for returning the dimensions
        """

        body = """
/**
 * Get dimensions of %(variable description)s
 * @param iobj reference to opaque plasma state object
 * @param ndims number of dimensions
 * @param dims dimensions
 * @param ierr error code (0=OK)
 */
#define %(C routine name)s F77NAME(%(routine name)s)
void %(C routine name)s(int* iobj, const int* ndims, int* dims, int* ierr); """ % prms
        print(body, file=f)

def write_getCharacterSizeCPrototype(f, prms):
        """
        Write C prototype for returning the string size
        """

        body = """
/**
 * Get character size of %(variable description)s
 * @param iobj reference to opaque plasma state object
 * @param size (out)
 * @param ierr error code (0=OK)
 */
#define %(C routine name)s F77NAME(%(routine name)s)
void %(C routine name)s(int* iobj, int* size, int* ierr); """ % prms
        print(body, file=f)


# maps the fortran type (taking the first 2 letters) to a C type
f2c = {
        're': 'double',
        'in' : 'int',
        'ch' : 'char',
        'lo' : 'int',
}
# maps the fortran type to C++ method descriptor
f2cxx = {
        're': 'Double',
        'in' : 'Int',
        'ch' : 'String',
        'lo' : 'Int',
}

def writeCCxxBodies(elem, fs, dig, fCheader, fCxxImpl, nm, dlist, ftype, cmts, getOnly):

        """
        Write all the C/C++ code necessary for getting/setting plasma_state members
        """

        # file handle
        #   change by DMC: distribute routines bts ccps_getset_a0.f90,
        #     ccps_getset_a1.f90, ... ..., ccps_getset_z9.f90

        k = nm[0].lower() + dig

        f = fs[k]

        # container for items to write
        prms = {}

        prms['member name'] = nm
        prms['lower case member name'] = nm.lower()

        # get method

        prms['routine name'] = 'psget_' + nm.lower()
        prms['C routine name'] = re.sub('psget_', 'psGet_', prms['routine name'])


        arglist = '(iobj, '
        nsizes  = []
        if dlist:
                nsizes       = [ '%s-%s+1' % (d[1],d[0]) for d in dlist ]

        dimlist = ''
        for i in range(len(nsizes)):
                dimlist += 'n%d ' % (i+1)
        arglist += re.sub(' ', ', ', dimlist) + 'data, ierr)'
        prms['arg list'] = arglist

        prms['data type'] = ftype
        prms['data size'] = ''
        m = re.match(r'^CHARACTER\*(\d+)', prms['data type'], re.I)
        if m:
                prms['data size'] = m.group(1)
                # make the character length variable, ie set by the last, invisible length
                # argument, to avoid filling of the string with bad characters
                prms['data type'] = re.sub(r'CHARACTER\*\d+', 'CHARACTER*(*)', prms['data type'])
        prms['C type'] = f2c[prms['data type'][0:2].lower()]
        prms['C++ type descr'] = f2cxx[prms['data type'][0:2].lower()]

        prms['data inout'] = ', intent(out)'

        prms['data rank'] = ''
        prms['data dims'] = ''
        prms['data dims decl'] = ''
        if len(nsizes) > 0:
                prms['data dims'] =  re.sub(' ', ', ', dimlist[:-1])
                prms['data rank'] = '(' + prms['data dims'] + ')'
                prms['data dims decl'] = 'integer, intent(in) :: ' + prms['data dims']
                prms['C++ type descr'] += '%dD' % len(nsizes)
        # remove [' ', ' and ']
        prms['variable description'] = re.sub(r"'\s*,\s*'", ", ", re.sub(r"'\]", "", re.sub(r"\['", "", repr(cmts))))
        prms['comment'] = breakComment('!! ' + prms['variable description'], maxlen=64, comment='    !! ')

        prms['data slice'] = re.sub(r'\(', '(1:', re.sub(r',', ', 1:', prms['data rank']))
        prms['left expr']  = 'data' + prms['data slice'] + ' = '
        prms['right expr'] = ''

        write_setget_body(f, prms)
        write_getCPrototype(fCheader, prms)
        write_getCxx(fCxxImpl, prms)

        # set method

        prms['routine name'] = 'psset_' + nm.lower()
        prms['C routine name'] = re.sub('psset_', 'psSet_', prms['routine name'])
        prms['data inout'] = ', intent(in)'

        prms['left expr']  = ''
        prms['right expr'] = ' = data' + prms['data slice']

        if not getOnly:
                write_setget_body(f, prms)
                write_setCPrototype(fCheader, prms)
                write_setCxx(fCxxImpl, prms)

        if prms['data size']:
                # for character strings
                prms['routine name'] = 'psgetsz_' + nm.lower()
                prms['C routine name'] = re.sub('psgetsz_', 'psGetSz_', prms['routine name'])
                write_getCharacterSize_body(f, prms)
                write_getCharacterSizeCPrototype(fCheader, prms)
                write_getCharacterSizeCxx(fCxxImpl, prms)


        if len(nsizes) > 0:

                # getNumDims method

                prms['routine name'] = 'psgetrnk_' + nm.lower()
                prms['C routine name'] = re.sub('psgetrnk_', 'psGetRnk_', prms['routine name'])
                prms['data numDims'] = len(nsizes)
                write_getNumDims_body(f, prms)
                write_getNumDimsCPrototype(fCheader, prms)
                write_getNumDimsCxx(fCxxImpl, prms)

                # getDims method

                prms['routine name'] = 'psgetdim_' + nm.lower()
                prms['C routine name'] = re.sub('psgetdim_', 'psGetDim_', prms['routine name'])
                write_getDims_body(f, prms)
                write_getDimsCPrototype(fCheader, prms)
                write_getDimsCxx(fCxxImpl, prms)


def plasma_cwrite(version_id):

        # MODIFIED dmc May 2010: write more sources each of smaller size:
        #   ccps_getset_a.f90 -> {ccps_getset_a0.f90, ccps_getset_a1.f90, ...
        #                         ccps_getset_a9.f90}
        #   (done for performance reasons of some fortran compilers)

        # all get/set routines will be written in files ccps_getset_<X><N>.f90
        #   where <X> is the first letter of the derived type member upon which
        #   the set/get methods acts, and <N> spreads the routine across the
        #   various source files.

        # construct sequence: a0, a1, a2, ... a9, b0 ...b9, ... ... , z0 ... z9

        fkeys = []
        for c in 'abcdefghijklmnopqrstuvwxyz':
                for dig in '0123456789':
                        cd = c + dig
                        fkeys.append(cd)

        # open a file for each sequence member
        fhndl = [open('ccps_getset_' + cd + '.f90', 'w') for cd in fkeys]

        # now associate each sequence member (key) to a file handle
        fs = dict(list(zip(fkeys, fhndl)))

        # C++ class implementation
        fCxxImpl = open('cxxps.cpp', 'w')
        print("""
// written by %s
// input: plasma_state_spec.dat   version %s
//  *** do not edit *** (use code generator instead)

// std includes
#include <iostream>

#include "cxxps.h"

PlasmaState::~PlasmaState() {
    int ierr = 0;
    psFree(_h, &ierr);
    if ( ierr != 0) {
        std::cerr << "PlasmaState::~PlasmaState: ERROR at line " << __LINE__ << " ierr=" << ierr << std::endl;
    }
}

PlasmaState::PlasmaState(const std::string& tag, const int debug) {

    int ierr = 0;
    psSetDebugLevel(&debug, &ierr);
    if ( ierr != 0) {
        std::cerr << "PlasmaState::PlasmaState: ERROR at line " << __LINE__ << " ierr=" << ierr << std::endl;
    }
    psInit(_h, tag.c_str(), &ierr, tag.size());
    if ( ierr != 0) {
        std::cerr << "PlasmaState::PlasmaState: ERROR at line " << __LINE__ << " ierr=" << ierr << std::endl;
    }

"""% ("plasma_cwrite.py", version_id), file=fCxxImpl)


        # hold C prototype functions
        fCheader = open('ccps.h', 'w')
        print("""
/* written by %s */
/* input: plasma_state_spec.dat   version %s */
/*  *** do not edit *** (use code generator instead) */

#ifndef CCPS_H
#define CCPS_H

#include "f77name.h"

/* include manually maintained methods */
#include "ccps_base.h"

#ifdef __cplusplus
extern "C" {
#endif
""" % ("plasma_cwrite.py", version_id), file=fCheader)


        # preamble
        for k in list(fs.keys()):
                print("""
! written by %s
! input: plasma_state_spec.dat   version %s
! *** do not edit *** (use code generator instead)

""" % ("plasma_cwrite.py", version_id), file=fs[k])

        for k in list(fs.keys()):
                f = fs[k]
                f.write("  subroutine ccps_getset_" + k + "_dummy\n")
                f.write("     return\n")
                f.write("  end subroutine ccps_getset_" + k + "_dummy\n")

        idig = -1
        for elem in all_items:
                idig = idig + 1
                if idig > 9:
                        idig = 0

                # distribute across source files 0,1,2,...,9:
                dig = repr(idig)

                nm = all_items[elem]['name']

                dlist = all_items[elem].get('dlist', '')
                ftype = all_items[elem]['ftype']
                cmts  = all_items[elem].get('cmts', '')
                writeCCxxBodies(elem=elem, dig=dig, fs=fs,
                                fCheader=fCheader, fCxxImpl=fCxxImpl, nm=nm,
                                dlist=dlist, ftype=ftype, cmts=cmts,
                                getOnly=False)

                if 'dlist' in all_items[elem] and \
                       all_items[elem].get('methkey', '') != "Hermite_derivative" \
                       and 'rank_coord' in all_items[elem]:
                        # write scalar ids
                        dlist = all_items[elem]['dlist']
                        rank_coord = all_items[elem]['rank_coord']
                        if rank_coord == len(dlist):
                                # nm looks like it is a profile
                                # need to provide get/set methods for
                                # corresponding id_<profile> numbers
                                # id_<profile> is needed to provide interpolation
                                # capability

                                #print '---> name = ', nm, ' has a scalar id '
                                cmts  = 'ID for %s profile' % nm
                                nm = 'id_' + nm.lower()
                                dlist = ''
                                ftype = 'INTEGER'
                                writeCCxxBodies(elem=elem, dig=dig, fs=fs,
                                                fCheader=fCheader, fCxxImpl=fCxxImpl,
                                                nm=nm, dlist=dlist, ftype=ftype, cmts=cmts,
                                                getOnly=True)
                        else:
                                # profile array; integer ID array
                                ### print '===> name = ', nm, ' has an array of ids '
                                cmts  = 'Array of IDs for %s profiles' % nm
                                nm = 'id_' + nm.lower()
                                ftype = 'INTEGER'
                                dlist = dlist[rank_coord:]
                                writeCCxxBodies(elem=elem, dig=dig, fs=fs,
                                                fCheader=fCheader, fCxxImpl=fCxxImpl,
                                                nm=nm, dlist=dlist, ftype=ftype, cmts=cmts,
                                                getOnly=True)

                                #print dlist[rank_coord:]


        print("""
#ifdef __cplusplus
}
#endif

#endif /* CCPS_H */
""", file=fCheader)
        print("""
}
""", file=fCxxImpl)
