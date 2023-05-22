# plasma_state

The Plasma State Component is a software component of the Integrated Plasma Simulator (IPS), being designed and built as a part of the SWIM (SciDAC) Fusion Simulation Project. It is envisioned as a shared repository for time evolving plasma simulation data - an instance of the plasma state contains a snapshot of the data at a single point in time. The Plasma State (PS) Component is also used for sharing of data across physics models in the IPS which are themselves implemented as separate, independently developed software components. Generally, data items which need to be shared between components are placed in the plasma state; data items used internally within a single component would typically not be in the plasma state. The PS is implemented as a fortran-95 derived data type, the elements of which are elementary fortran types: 64 bit REAL, INTEGER, and CHARACTER*nn scalars and arrays. The type definition and supporting input/ output routines are written from a specification file plasma_state_spec.dat by a python script code generator. Implementing software provides for interpolation and "conservative rezoning" of profiles contained in the Plasma State, as will be described. Multiple, separately named state instances can be held in memory simultaneously.

## Building

If your platform is not included then following the examples in the `environ` script and the `environs/` directory.
```
source environ
make
```
Cleaning up old builds can be done with
```
make clean  # Deletes objects and modules
make clobber/realclean  # Deletes objects, modules, libs, exes
```
To build only the libraries or executables:
```
make libs
make exes
```
The builds will get installed in a directory such as:
```
builds/{COMPILER}/{COMPILER_VERSION}
```
where the compiler and compiler version are specified by the environs.
