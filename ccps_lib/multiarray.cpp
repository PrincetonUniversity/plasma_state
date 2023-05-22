/**
 * $Id: multiarray.cpp,v 1.2 2008-02-22 23:59:57 Alex_Pletzer Exp $
 *
 */

// local includes
#include "multiarray.h"

// std includes 
#include <iostream>

template <typename TYPE>
MultiArray<TYPE>::MultiArray() {
  _ntot = 0;
}

template <typename TYPE>
MultiArray<TYPE>::~MultiArray<TYPE>() {
}

template <typename TYPE> void 
MultiArray<TYPE>::setDims(const std::vector<size_t>& szs) {
  _sizes.resize( szs.size() );
  _ntot = 1;
  for (size_t i = 0; i < szs.size(); ++i) {
    _sizes[i] = szs[i];
    _ntot *= szs[i];
  }
  _data.resize(_ntot);
}

template <typename TYPE> std::vector<size_t>
MultiArray<TYPE>::getDims()  const {
  return _sizes;
}

template <typename TYPE> size_t
MultiArray<TYPE>::getSize() const {
  return _ntot;
}

template <typename TYPE> void
MultiArray<TYPE>::setData(const std::vector<TYPE>& dta) {
  if (dta.size() != _ntot) {
    std::cerr << "MultiArray::setData: ERROR size mismatch " <<
      dta.size() << " != " << _ntot << std::endl;
  }
  else {
    for (size_t i = 0; i < dta.size(); ++i) {
    _data[i] = dta[i];
    }
  }
}

template <typename TYPE> const TYPE*
MultiArray<TYPE>::getDataPointer() const {
  return &_data[0];
}

template <typename TYPE> TYPE&
MultiArray<TYPE>::operator[] (const size_t k){
  return _data[k];
}

template <typename TYPE> const TYPE&
MultiArray<TYPE>::operator[] (const size_t k) const{
  return _data[k];
}
template <typename TYPE> TYPE&
MultiArray<TYPE>::operator[] (const std::vector<size_t>& indices){
  size_t k = getGlobalIndex(indices);
  return _data[k];
}

template <typename TYPE> size_t
MultiArray<TYPE>::getGlobalIndex(const std::vector<size_t>& indices) const {
  size_t k = 0;
  size_t n = 1;
  for (size_t i = 0; i < _sizes.size(); ++i) {
    k += indices[i] * n;
    n *= _sizes[i];
  }
  return k;
}

// instantiations
template class MultiArray<double>;
template class MultiArray<int>;
template class MultiArray<char>;
