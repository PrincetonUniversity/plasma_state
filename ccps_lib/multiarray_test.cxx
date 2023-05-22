/**
 * $Id: multiarray_test.cxx,v 1.2 2008-02-26 17:43:30 Alex_Pletzer Exp $
 * Unit test for multi-dimensional arrays
 */

#include <iostream>
#include <assert.h>

#include "multiarray.h"

int main() {

  // dimensionality
  const size_t NDIM = 3;

  MultiArray<double> a, b;

  // sizes along each dimension
  std::vector<size_t> szs(NDIM);
  szs[0] = 2;
  szs[1] = 3;
  szs[2] = 4;
  a.setDims(szs);
  b.setDims(szs);

  // set data
  size_t ntot = 1;
  for (size_t i = 0; i < szs.size(); ++i) 
    ntot *= szs[i];
  std::vector<double> data(ntot);
  std::vector<size_t> indices(NDIM);
  size_t el = 0;

  // two ways of setting data, element by element
  // or just loading a flat array

  for (size_t k = 0; k < szs[2]; ++k) {
    indices[2] = k;
    for (size_t j = 0; j < szs[1]; ++j) {
      indices[1] = j;
      for (size_t i = 0; i < szs[0]; ++i) {
        indices[0] = i;
        data[el] = i + szs[0] * (j + szs[1] * k);
        b[indices] = data[el];
        el += 1;
      }
    }
  }
  a.setData(data);

  // checking copy-ctor
  // and assignment
  MultiArray<double> c1(a);
  MultiArray<double> c2 = a;
  

  // check
  for (size_t k = 0; k < szs[2]; ++k) {
    indices[2] = k;
    for (size_t j = 0; j < szs[1]; ++j) {
      indices[1] = j;
      for (size_t i = 0; i < szs[0]; ++i) {
        indices[0] = i;
        std::cout << i << j << k << " a=" << a[indices] << 
          " b=" << b[indices] << '\n';
        assert( b[indices] == a[indices]);
        assert( c1[indices] == a[indices]);
        assert( c2[indices] == a[indices]);
        
      }
    }
  }
  
  return 0;
}
