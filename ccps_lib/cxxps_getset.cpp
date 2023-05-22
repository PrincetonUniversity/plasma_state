/**
 * $Id: cxxps_getset.cpp 855 2011-02-18 20:03:30Z pletzer $
 * Implementation of get/set PlasmaState methods. The methods are overloaded 
 * for int's, double's, strings and arrays of the formers. 
 * Arrays of ranks up to 3 are implemented via setData and getData methods. 
 * 
 */
// std includes
#include <iostream>
#include <limits>

// local includes
#include "cxxps.h"
#include "multiarray.h"

#define DEBUG

// typedef's

typedef std::map<std::string, void (*)(int*, const int*, int*) >::iterator iterSetInt;
typedef std::map<std::string, void (*)(int*, const int*, const int*, int*) >::iterator iterSetInt1D;
typedef std::map<std::string, void (*)(int*, const int*, const int*, const int*, int*) >::iterator iterSetInt2D;
typedef std::map<std::string, void (*)(int*, const int*, const int*, const int*, const int*, int*) >::iterator iterSetInt3D;

typedef std::map<std::string, void (*)(int*, const double*, int*) >::iterator iterSetDouble;
typedef std::map<std::string, void (*)(int*, const int*, const double*, int*) >::iterator iterSetDouble1D;
typedef std::map<std::string, void (*)(int*, const int*, const int*, const double*, int*) >::iterator iterSetDouble2D;
typedef std::map<std::string, void (*)(int*, const int*, const int*, const int*, const double*, int*) >::iterator iterSetDouble3D;

typedef std::map<std::string, void (*)(int*, const char*, int*, size_t) >::iterator iterSetString;
typedef std::map<std::string, void (*)(int*, const int*, const char*, int*, size_t) >::iterator iterSetString1D;

int*
PlasmaState::getHandle() {
  return _h;
}

void
PlasmaState::setData(const std::string& nm, const double vl) {
  iterSetDouble i = _setDouble.find(nm);
  if( i == _setDouble.end() ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 1);
  }
  else {
    int ierr = 0;
    i->second(_h, &vl, &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 2);
    }
  }  
}

void
PlasmaState::setData(const std::string& nm, const int vl) {
  iterSetInt i = _setInt.find(nm);
  if( i == _setInt.end() ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 3);
  }
  else {
    int ierr = 0;
    i->second(_h, &vl, &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr << 
	  " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 4);
    }
  }  
}

void
PlasmaState::setData(const std::string& nm, const std::string& vl) {
  iterSetString i = _setString.find(nm);
  if( i == _setString.end() ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 5);
  }
  else {
    int ierr = 0;
    i->second(_h, vl.c_str(), &ierr, vl.size());
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 6);
    } 
  } 
}

void
PlasmaState::setData(const std::string& nm, const std::vector<double>& vl) {
  iterSetDouble1D i = _setDouble1D.find(nm);
  if( i == _setDouble1D.end() ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 7);
  }
  else {
    size_t n1 = this->getDims(nm)[0];
    if (vl.size() != n1) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " incompatible sizes=" << 
	  vl.size() << " != " << n1 << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 8);
    }
    else {
      int ierr = 0;
      int vs = vl.size();
      i->second(_h, &vs, &vl[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 9);
      }
    } 
  } 
}

void
PlasmaState::setData(const std::string& nm, const std::vector<int>& vl) {
  iterSetInt1D i = _setInt1D.find(nm);
  if( i == _setInt1D.end() ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 10);
  }
  else {
    size_t n1 = this->getDims(nm)[0];
    if (vl.size() != n1) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " incompatible sizes=" << 
	  vl.size() << " != " << n1 << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 11);
    }
    else {
      int ierr = 0;
      int vs = vl.size();
      i->second(_h, &vs, &vl[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 12);
      }  
    }
  }
}

void
PlasmaState::setData(const std::string& nm, const std::vector<std::string>& vl) {
  iterSetString1D i = _setString1D.find(nm);
  if( i == _setString1D.end() ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 13);
  }
  else {
    // character length on Fortran side
    size_t sz = this->getCharSize(nm);
    size_t n1 = this->getDims(nm)[0];
    if (vl.size() != n1) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " incompatible sizes=" << 
	  vl.size() << " != " << n1 << " for nm=" << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 14);
    }
    else {
      int ierr = 0;
      // load array into a contiguous place holder
      std::string vlFlat(n1 * sz, ' ');
      for (size_t j = 0; j < n1; ++j) {
	for (size_t k = 0; k < vl[j].size(); ++k) {
	  vlFlat[j*sz + k] = vl[j][k];
	}
      }
      int n1Int = n1; // cast into an int
      i->second(_h, &n1Int, vlFlat.c_str(), &ierr, sz);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 15);
      } 
    }
  } 
}

void
PlasmaState::setData(const std::string& nm, const MultiArray<double>& vl) {
  std::vector<size_t> dims = vl.getDims();
  size_t ndims = dims.size();
  int ierr = 0;
  iterSetDouble1D i1d;
  iterSetDouble2D i2d;
  iterSetDouble3D i3d;
  switch (ndims) {
  case 1:
    i1d = _setDouble1D.find(nm);
    if(i1d == _setDouble1D.end()) {
      std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 16);
    }
    else {
      int n1 = dims[0];
      i1d->second(_h, &n1, vl.getDataPointer(), &ierr);
    }
    break;
  case 2:
    i2d = _setDouble2D.find(nm);
    if(i2d == _setDouble2D.end()) {
      std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 17);
    }
    else {
      int n1 = dims[0];
      int n2 = dims[1];
      i2d->second(_h, &n1, &n2, vl.getDataPointer(), &ierr);
    }
    break;
  case 3:
    i3d = _setDouble3D.find(nm);
    if(i3d == _setDouble3D.end()) {
      std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 18);
    }
    else {
      int n1 = dims[0];
      int n2 = dims[1];
      int n3 = dims[2];
      i3d->second(_h, &n1, &n2, &n3, vl.getDataPointer(), &ierr);
    }    
    break;
  default:
    std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << ' ' << ndims 
	    << "-dimensional arrays are not supported\n";
    ierr = -1;
    throw PlasmaStateError(__FILE__, __LINE__, 19);    
  }
  if (ierr != 0) {
    std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
	      << " for nm=" << nm << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 20);
  }
}

void
PlasmaState::setData(const std::string& nm, const MultiArray<int>& vl) {
  std::vector<size_t> dims = vl.getDims();
  size_t ndims = dims.size();
  int ierr = 0;
  iterSetInt1D i1d;
  iterSetInt2D i2d;
  iterSetInt3D i3d;
  switch (ndims) {
  case 1:
    i1d = _setInt1D.find(nm);
    if(i1d == _setInt1D.end()) {
      std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 21);
    }
    else {
      int n1 = dims[0];
      i1d->second(_h, &n1, vl.getDataPointer(), &ierr);
    }
    break;
  case 2:
    i2d = _setInt2D.find(nm);
    if(i2d == _setInt2D.end()) {
      std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 22);
    }
    else {
      int n1 = dims[0];
      int n2 = dims[1];
      i2d->second(_h, &n1, &n2, vl.getDataPointer(), &ierr);
    }
    break;
  case 3:
    i3d = _setInt3D.find(nm);
    if(i3d == _setInt3D.end()) {
      std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 23);
    }
    else {
      int n1 = dims[0];
      int n2 = dims[1];
      int n3 = dims[2];
      i3d->second(_h, &n1, &n2, &n3, vl.getDataPointer(), &ierr);
    }    
    break;
  default:
    std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << ' ' << ndims 
	    << "-dimensional arrays are not supported\n";
    ierr = -1;
    throw PlasmaStateError(__FILE__, __LINE__, 24);
  }
  if (ierr != 0) {
    std::cerr << "PlasmaState::setData: ERROR at line " << __LINE__ << " ierr=" << ierr 
	      << " for nm=" << nm << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 25);
  }
}

template <typename T>
T getData(const std::string& nm) {
    std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << 
        " invalid template type, use eg get<double>(...)" << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 26);
}


template <> 
double
PlasmaState::getData<double>(const std::string& nm) {
#ifdef DEBUG
    if ( _getDouble.find(nm) == _getDouble.end() ) {
        std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 27);
    }
#endif
    double res;
    int ierr = 0;
    _getDouble[nm](_h, &res, &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << " ierr=" << ierr << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 28);
    }  
    return res;
}

template <> 
int
PlasmaState::getData<int>(const std::string& nm) {
#ifdef DEBUG
    if ( _getInt.find(nm) == _getInt.end() ) {
        std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 29);
    }
#endif
    int res;
    int ierr = 0;
    _getInt[nm](_h, &res, &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 30);
    }  
    return res;
}

template <> 
std::string
PlasmaState::getData<std::string>(const std::string& nm) {
#ifdef DEBUG
    if ( _getString.find(nm) == _getString.end() ) {
        std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 31);
    }
#endif
    size_t sz = this->getCharSize(nm);
    char resFlat[sz];
    int ierr = 0;
    _getString[nm](_h, resFlat, &ierr, sz);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::get: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 32);
    }  
    std::string res;
    res.assign(resFlat, sz);
    return res;
}

template <> 
std::vector<double>
PlasmaState::getData< std::vector<double> >(const std::string& nm) {
  
  std::vector<size_t> dims = this->getDims(nm);
  size_t ndims = dims.size();
  // will return data as a flat array of nelem elements
  int nelem = dims[0];
  for (size_t i = 1; i < dims.size(); ++i) {
    nelem *= dims[i];
  }
  std::vector<double> res(nelem);
  int ierr = 0;

  switch (ndims) {

  case 1:
    if ( _getDouble1D.find(nm) != _getDouble1D.end() ) {
      int n1 = dims[0];
      _getDouble1D[nm](_h, &n1, &res[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for 1D field nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 33);
      }
    }
    else {
      std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
	" nm=" << nm << " not found among 1D fields\n";
      throw PlasmaStateError(__FILE__, __LINE__, 34);
    }
    break;

  case 2:
    if ( _getDouble2D.find(nm) != _getDouble2D.end() ) {
      int n1 = dims[0];
      int n2 = dims[1];
      _getDouble2D[nm](_h, &n1, &n2, &res[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for 2D field nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 35);
      }
    }
    else {
      std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
	" nm=" << nm << " not found among 2D fields\n";
      throw PlasmaStateError(__FILE__, __LINE__, 36);
    }
    break;

  case 3:
    if ( _getDouble3D.find(nm) != _getDouble3D.end() ) {
      int n1 = dims[0];
      int n2 = dims[1];
      int n3 = dims[2];
      _getDouble3D[nm](_h, &n1, &n2, &n3, &res[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for 3D field nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 37);
      }
    }
    else {
      std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
	" nm=" << nm << " not found among 3D fields\n";
      throw PlasmaStateError(__FILE__, __LINE__, 38);
    }
    break;

  default:
    std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
      " invalid number of dimensions= " << ndims << '\n';
    throw PlasmaStateError(__FILE__, __LINE__, 39);
  }
    return res;
}

template <> 
std::vector<std::string>
PlasmaState::getData< std::vector<std::string> >(const std::string& nm) {
  // only 1D string array data supported
#ifdef DEBUG
    if ( _getString1D.find(nm) == _getString1D.end() ) {
        std::cerr << "PlasmaState::get1D: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 40);
    }
#endif
    std::vector<size_t> dims = this->getDims(nm);
    int n = dims[0];
    for (size_t i = 1; i < dims.size(); ++i) n *= dims[i];
    int ierr = 0;
    // contiguous place holder
    size_t sz = this->getCharSize(nm);
    size_t ntot = n * sz;
    char resFlat[ ntot ];
    _getString1D[nm](_h, &n, resFlat, &ierr, sz);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::get1D: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 41);
    }
    std::vector<std::string> res(n);
    for (size_t i = 0; i < res.size(); ++i) {
      res[i].assign(&resFlat[i * sz], sz);
    }
    return res;
}

template <> 
std::vector<int>
PlasmaState::getData< std::vector<int> >(const std::string& nm) {
  
  std::vector<size_t> dims = this->getDims(nm);
  size_t ndims = dims.size();
  // will return data as a flat array of nelem elements
  int nelem = dims[0];
  for (size_t i = 1; i < dims.size(); ++i) {
    nelem *= dims[i];
  }
  std::vector<int> res(nelem);
  int ierr = 0;

  switch (ndims) {

  case 1:
    if ( _getInt1D.find(nm) != _getInt1D.end() ) {
      int n1 = dims[0];
      _getInt1D[nm](_h, &n1, &res[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for 1D field nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 42);
      }
    }
    else {
      std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
	" nm=" << nm << " not found among 1D fields\n";
      throw PlasmaStateError(__FILE__, __LINE__, 43);
    }
    break;

  case 2:
    if ( _getInt2D.find(nm) != _getInt2D.end() ) {
      int n1 = dims[0];
      int n2 = dims[1];
      _getInt2D[nm](_h, &n1, &n2, &res[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for 2D field nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 44);
      }
    }
    else {
      std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
	" nm=" << nm << " not found among 2D fields\n";
      throw PlasmaStateError(__FILE__, __LINE__, 45);
    }
    break;

  case 3:
    if ( _getInt3D.find(nm) != _getInt3D.end() ) {
      int n1 = dims[0];
      int n2 = dims[1];
      int n3 = dims[2];
      _getInt3D[nm](_h, &n1, &n2, &n3, &res[0], &ierr);
      if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << " ierr=" << ierr 
		  << " for 3D field nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 46);
      }
    }
    else {
      std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
	" nm=" << nm << " not found among 3D fields\n";
      throw PlasmaStateError(__FILE__, __LINE__, 47);
    }
    break;

  default:
    std::cerr << "PlasmaState::getData: ERROR at line " << __LINE__ << 
      " invalid number of dimensions= " << ndims << '\n';
    throw PlasmaStateError(__FILE__, __LINE__, 48);
  }
    return res;
}

template <> 
MultiArray<double> 
PlasmaState::getData< MultiArray<double> >(const std::string& nm) {
  
  std::vector<size_t> dims = this->getDims(nm);
  MultiArray<double> res;
  res.setDims(dims);
  res.setData( this->getData<std::vector<double> >(nm) );
  return res;
}

std::vector<size_t> 
PlasmaState::getDims(const std::string& nm) {
#ifdef DEBUG
    if ( _getNumDims.find(nm) == _getNumDims.end() || 
         _getDims.find(nm) == _getDims.end() ) {
        std::cerr << "PlasmaState::getDims: ERROR at line " << __LINE__ << " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 49);
    }
#endif
    int ndims;
    int ierr = 0;
    _getNumDims[nm](_h, &ndims, &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getDims: ERROR at line " << __LINE__ << " after calling getNumDims for " << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 50);
    }
    std::vector<int> vs(ndims);
    _getDims[nm](_h, &ndims, &vs[0], &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getDims: ERROR at line " << __LINE__ << " after calling getDims for " << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 51);
    }
    // cast into size_t
    std::vector<size_t> dims(ndims);
    for (int i = 0; i < ndims; ++i) dims[i] = vs[i];
    return dims;
}


size_t 
PlasmaState::getCharSize(const std::string& nm) {
#ifdef DEBUG
    if ( _getCharacterSize.find(nm) == _getCharacterSize.end() ) {
        std::cerr << "PlasmaState::getCharacterSize: ERROR at line " << __LINE__ << 
          " invalid nm=" << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 52);
    }
#endif
    int n;
    int ierr = 0;
    _getCharacterSize[nm](_h, &n, &ierr);
    if ( ierr != 0 ) {
        std::cerr << "PlasmaState::getCharacterSize: ERROR at line " << __LINE__ << 
          " after calling getDims for " << nm << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 53);
    }
    return (size_t) n;
}

std::string
PlasmaState::getVersionId() {
  char version[32];
  int sz;
  int ierr = 0;
  psGetVersionId(version, &sz, &ierr, 32);
  if (sz != 32) {
    std::cerr << "PlasmaState::getVersionId: ERROR at line " << __LINE__ << 
      " version_id size seems to have changed (" << sz << " != 32)!\n";
    throw PlasmaStateError(__FILE__, __LINE__, 54);
  }
  std::string res;
  res.insert(0, version, 32);
  return res;
}

void
PlasmaState::alloc() {
  int ierr = 0;
  psAlloc(_h, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::alloc: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 55);
  }
}

void
PlasmaState::setThermalSpecies(const int zAtom, const int zCharge, const int amu) {
  double aC, iC, ms;
  int ierr = 0;
  psSpeciesConvert(&zAtom, &zCharge, &amu, &aC, &iC, &ms, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::setThermalSpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 55);
  }
  else {
    _thAtomicCharges.push_back(aC);
    _thIonicCharges.push_back(iC);
    _thMasses.push_back(ms);
  }
}

void
PlasmaState::setNeutralBeamSpecies(const int zAtom, const int zCharge, const int amu) {
  double aC, iC, ms;
  int ierr = 0;
  psSpeciesConvert(&zAtom, &zCharge, &amu, &aC, &iC, &ms, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::setNeutralBeamSpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 56);
  }
  else {
    _nbAtomicCharges.push_back(aC);
    _nbIonicCharges.push_back(iC);
    _nbMasses.push_back(ms);
  }
}

void
PlasmaState::setRFMinoritySpecies(const int zAtom, const int zCharge, const int amu) {
  double aC, iC, ms;
  int ierr = 0;
  psSpeciesConvert(&zAtom, &zCharge, &amu, &aC, &iC, &ms, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::setRFMinoritySpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 57);
  }
  else {
    _rfAtomicCharges.push_back(aC);
    _rfIonicCharges.push_back(iC);
    _rfMasses.push_back(ms);
  }
}

void
PlasmaState::setFusionSpecies(const int zAtom, const int zCharge, const int amu) {
  double aC, iC, ms;
  int ierr = 0;
  psSpeciesConvert(&zAtom, &zCharge, &amu, &aC, &iC, &ms, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::setFusionSpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 58);
  }
  else {
    _fsAtomicCharges.push_back(aC);
    _fsIonicCharges.push_back(iC);
    _fsMasses.push_back(ms);
  }
}

void
PlasmaState::setImpuritySpecies(const int zAtom, const int zCharge, const int amu) {
  double aC, iC, ms;
  int ierr = 0;
  psSpeciesConvert(&zAtom, &zCharge, &amu, &aC, &iC, &ms, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::setImpuritySpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 77);
  }
  else {
    _imAtomicCharges.push_back(aC);
    _imIonicCharges.push_back(iC);
    _imMasses.push_back(ms);
  }
}

void
PlasmaState::finishSpecies() {

  // number of thermal species does not include electrons
  this->setData("nspec_th", (int) _thAtomicCharges.size() - 1);

  this->setData("nspec_beam", (int) _nbAtomicCharges.size());
  this->setData("nspec_rfmin", (int) _rfAtomicCharges.size());
  this->setData("nspec_fusion", (int) _fsAtomicCharges.size());
  this->setData("nspec_impi", (int) _imAtomicCharges.size());

  this->alloc();

  this->setData("qatom_s", _thAtomicCharges);
  this->setData("q_s", _thIonicCharges);
  this->setData("m_s", _thMasses);

  this->setData("qatom_snbi", _nbAtomicCharges);
  this->setData("q_snbi", _nbIonicCharges);
  this->setData("m_snbi", _nbMasses);

  this->setData("qatom_rfmin", _rfAtomicCharges);
  this->setData("q_rfmin", _rfIonicCharges);
  this->setData("m_rfmin", _rfMasses);

  this->setData("qatom_sfus", _fsAtomicCharges);
  this->setData("q_sfus", _fsIonicCharges);
  this->setData("m_sfus", _fsMasses);

  this->setData("qatom_simpi", _imAtomicCharges);
  this->setData("q_simpi", _imIonicCharges);
  this->setData("m_simpi", _imMasses);

  int ierr = 0;
  psLabelSpecies(_h, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::finishSpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 59);
  }
  psMergeSpeciesLists(_h, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::finishSpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 60);
  }  
  psNeutralSpecies(_h, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::finishSpecies: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 61);
  }  
}
        
void 
PlasmaState::updateEquilibrium(const std::string g_filepath, const double bdy_crat,
			       const int kcur_option, const double rho_curbrk) {

  int ierr = 0;
  const int npprime = 0;
  const double null = 0;
  psUpdateEquilibrium(_h, g_filepath.c_str(), &bdy_crat, 
		      &kcur_option, &rho_curbrk, 
		      &npprime, &null, &null, &ierr, g_filepath.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::updateEquilibrium: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 62);
  }
}

double 
PlasmaState::verifyMhdEq(const std::vector<double>& rhowk, const double reltol, 
     const bool update_phit, const bool update_psi, const bool update_q, 
     const bool check_phit, const bool check_q) {

  double relerr_max;
  int nrhowk = rhowk.size();

  int update_phit_int = 0;
  if (update_phit) update_phit_int = 1;
  int update_psi_int  = 0;
  if (update_psi) update_psi_int = 1;
  int update_q_int    = 0;
  if (update_q) update_q_int = 1;
  int check_phit_int  = 1;
  if (!check_phit) check_phit_int = 0;
  int check_q_int     = 1;
  if (!check_q) check_q_int = 0;
  
  int ierr;
  psMhdeqVerify(_h, &reltol, &nrhowk, &rhowk[0], 
       &update_phit_int, &update_psi_int, &update_q_int,
       &check_phit_int, &check_q_int, &relerr_max, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::verifyMhdEq: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 63);
  }
  return relerr_max;
}


void 
PlasmaState::deriveMhdEq(const std::string& action, const std::vector<double>& rhowk) {

  int nrhowk = rhowk.size();
  int ierr;
  psMhdeqDerive(_h, action.c_str(), &nrhowk, &rhowk[0], &ierr, action.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::deriveMhdEq: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 64);
  }
}

void 
PlasmaState::readMachDescr(const std::string& filepath, 
			   const std::string& action, 
			   const std::string& g_filepath) {

  int ierr;
  psMDescrRead(_h, filepath.c_str(), action.c_str(), g_filepath.c_str(), 
	       &ierr, filepath.size(), action.size(), g_filepath.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::readMachDescr: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 65);
  }
}

void 
PlasmaState::writeMachDescr(const std::string& filename) {

  int ierr;
  psMDescrWrite(_h, filename.c_str(), &ierr, filename.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::writeMachDescr: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 66);
  }
}

void 
PlasmaState::writeGeqdsk(const std::string& filename) {

  int ierr;
  psGeqdskWrite(_h, filename.c_str(), &ierr, filename.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::writeGeqdsk: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 67);
  }
}

void 
PlasmaState::updateHashCodes(const bool mdescr_flag, const bool sconfig_flag, 
			     const bool simInit_flag) {
  int mdescr_iflag = 0;
  if (mdescr_flag) mdescr_iflag = 1;
  int sconfig_iflag = 0;
  if (sconfig_flag) sconfig_iflag = 1;
  int simInit_iflag = 0;
  if (simInit_flag) simInit_iflag = 1;

  int ierr;
  psUpdateHashCodes(_h, &mdescr_iflag, &sconfig_iflag, &simInit_iflag, &ierr);
  if (ierr != 0) {
    std::cerr << "PlasmaState::updateHashCodes: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 68);
  }
}

void 
PlasmaState::readShotConfig(const std::string& filepath) {

  int ierr;
  psSConfigRead(_h, filepath.c_str(), &ierr, filepath.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::readShotConfig: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 69);
  }
}

void 
PlasmaState::writeShotConfig(const std::string& filepath) {

  int ierr;
  psSConfigWrite(_h, filepath.c_str(), &ierr, filepath.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::writeShotConfig: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 70);
  }
}

void 
PlasmaState::store(const std::string& filepath) {

  int ierr;
  psStorePlasmaState(_h, filepath.c_str(), &ierr, filepath.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::store: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 71);
  }

}

int 
PlasmaState::hashDiff(const std::string& lbl1, 
	      PlasmaState& obj2, const std::string& lbl2,
		      const std::string& sstr, int* icomp) {

  int idiff;
  psHashDiff(_h, lbl1.c_str(), obj2.getHandle(), lbl2.c_str(),
		  sstr.c_str(), icomp, &idiff, 
	          lbl1.size(), lbl2.size(), sstr.size());
  return idiff;
}

void 
PlasmaState::getState(const std::string& filepath) {

  int ierr;
  psGetPlasmaState(_h, filepath.c_str(), &ierr, filepath.size());
  if (ierr != 0) {
    std::cerr << "PlasmaState::getState: ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 72);
  }
}

std::vector<double>
PlasmaState::interp1d(const std::string& nm, const int deriv, 
			const std::vector<double>& xs) {

  int ierr = 0;
  int n = xs.size();
  std::vector<double> res(n, std::numeric_limits<double>::min());
  std::string idName = std::string("id_") + nm;
  int id = this->getData<int>(idName);
  psIntrp1dVector(_h, &id, &deriv, &n, &xs[0], &res[0], &ierr);
  if (ierr != 0) {
      std::cerr << "PlasmaState::interp1d: ERROR at line " << __LINE__ << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 73);
  }
  return res;
}

std::vector<double>
PlasmaState::interp1d(const std::string& nm, const int deriv, 
		      const std::vector<double>& xs, const size_t index) {

  int ierr = 0;
  int n = xs.size();
  std::vector<double> res(n, std::numeric_limits<double>::min());
  std::string idName = std::string("id_") + nm;
  std::vector<int> ids = this->getData<std::vector<int> >(idName);
  psIntrp1dVector(_h, &ids[index], &deriv, &n, &xs[0], &res[0], &ierr);
  if (ierr != 0) {
      std::cerr << "PlasmaState::interp1d: ERROR at line " << __LINE__ << std::endl;
      throw PlasmaStateError(__FILE__, __LINE__, 73);
  }
  return res;
}

std::vector<double>
PlasmaState::interp2d(const std::string& nm, const int deriv1, const int deriv2,
		      const std::vector<double> x1s, const std::vector<double> x2s) {

  int ierr = 0;
  int n = x1s.size();
  std::vector<double> res(n, std::numeric_limits<double>::min());
  if (n != (int) x2s.size()) {
    std::cerr << "PlasmaState::interp2d: size of x1s, x2s do not match. ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 74);
  }
  else {
    std::string idName = std::string("id_") + nm;
    int id = this->getData<int>(idName);
    psIntrp2dVector(_h, &id, &deriv1, &deriv2, &n, &x1s[0], &x2s[0], &res[0], &ierr);
    if (ierr != 0) {
	std::cerr << "PlasmaState::interp2d: ERROR at line " << __LINE__ << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 75);
    }
  }
  return res;
}

std::vector<double>
PlasmaState::interp2d(const std::string& nm, const int deriv1, const int deriv2,
		      const std::vector<double> x1s, const std::vector<double> x2s, const size_t index) {

  int ierr = 0;
  int n = x1s.size();
  std::vector<double> res(n, std::numeric_limits<double>::min());
  if (n != (int) x2s.size()) {
    std::cerr << "PlasmaState::interp2d: size of x1s, x2s do not match. ERROR at line " << __LINE__ << std::endl;
    throw PlasmaStateError(__FILE__, __LINE__, 74);
  }
  else {
    std::string idName = std::string("id_") + nm;
    std::vector<int> ids = this->getData<std::vector<int> >(idName);
    psIntrp2dVector(_h, &ids[index], &deriv1, &deriv2, &n, &x1s[0], &x2s[0], &res[0], &ierr);
    if (ierr != 0) {
	std::cerr << "PlasmaState::interp2d: ERROR at line " << __LINE__ << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 75);
    }
  }
  return res;
}

void
PlasmaState::writeUpdateFile(const std::string& filename, const int hashflag) {

  int ierr = 0;
  psWriteUpdateFile(_h, filename.c_str(), &hashflag, 
		      &ierr, filename.size());
  if (ierr != 0) {
	std::cerr << "PlasmaState::writeUpdateFile: ERROR at line " << 
	  __LINE__ << std::endl;
	throw PlasmaStateError(__FILE__, __LINE__, 76);
  }  
}

#undef DEBUG
