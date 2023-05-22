#
#  for a DEC/Compaq alpha...
#
#   NTCC libraries
   set libdir = /usr/ntcc/lib
#
#   NTCC f90 "public" modules
   set moddir = /usr/ntcc/mod
#
#   for the DEC/Compaq alpha linux workstation...
#
   set f90opt = "fort -fast -assume no2underscore"
   set f90opt = "$f90opt -I$moddir"
#
#  MDSplus is not linked.  In this program "-lnetcdf -llapack"
#  and "-lreadline -lhistory -ltermcap" could be omitted; -lblas is needed.
#  however, related programs will need netcdf and lapack, and, the gnu
#  libraries -lreadline, etc., enable command line editing / recall inside
#  interactive fortran programs, which is useful.
#
   $f90opt  geqxpl2.for -L $libdir -lxplasma \
                      -lfluxav -lmclib -lsmlib -ltrgraf \
                      -lureadsub -lsg -ljc -lnscrunch -lr8bloat -lfftsub \
                      -lcomput -lvaxonly -lpspline -lezcdf -llsode  \
                      -llsode_linpack -lportlib \
                   -L/usr/local/lib \
                      -lnetcdf -llapack -lblas \
                      -lreadline -lhistory -ltermcap
