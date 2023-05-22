/**
 * C++ test for plasma state interface
 * $Id: ccps_test_geqdsk.cxx,v 1.4 2009-01-12 23:57:44 xshare Exp $
 */

// std includes
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <assert.h>
#include <cmath>

// incl_cpp and ccps includes
#include "f77name.h"
#include "ccps_base.h"
#include "ccps.h"
#include "cxxps.h"

#include <cstdio>

/**
 * error checking
 */
void checkError(const int lineNumber, const int ier) {
  if (ier != 0) std::cerr << "***Error occurred at line " << 
    lineNumber << '\n';
}

/**
 * Construct a a uniform grid
 */
std::vector<double> linearGrid(const double xmin, const double xmax, const size_t n1) {
  double dx = (xmax - xmin) / double(n1 - 1);
  std::vector<double> xs(n1);
  for (size_t i = 0; i < n1; ++i) {
    xs[i] = xmin + dx * i;
  }
  return xs;
}

/**
 * return debug code from command line arguments
 */
int getDebugLevel(int argc, char* argv[]) {
  int res = 0;
  for (int ia = 1; ia < argc; ++ia) {
    std::string arg(argv[ia]);
    size_t i = arg.find("debug=", 0);
    if (i != std::string::npos) {
      // found command line argument "debug="
      // convert char to int
      std::istringstream istrm( arg.substr( arg.find("=", 0) + 1) );
      istrm >> res;
      break;
    }
  }
  return res;
}


/** 
 * Test C++ interface
 *
 */
void testCxx(int argc, char* argv[]) {

  int debug = getDebugLevel(argc, argv);
  PlasmaState ps("test", debug);

  // this grid's resolution
  int ns1 = 11;
  ps.setData("nrho_eq", ns1);
  int nt1 = 33;
  ps.setData("nth_eq", nt1);

  // All grids must be defined before state can be written to file
  std::cout << "-- building plasma state equilibrium from g file -- \n";
  ps.updateEquilibrium("g104403.nstx", 0.075001, 0, 0.9);

  std::cout << "  Equilibrium dimensions (rho)x(theta) :" << ns1 << " x " << nt1 << '\n';

  // equilibrium will be interpolated on this grid size
  ps.setData("nrho_eq_geo",  ns1);
  ps.alloc();
  std::vector<double> rho_eq = ps.getData<std::vector<double> >("rho_eq");
  ps.setData("rho_eq_geo", rho_eq);
  std::vector<double> th_eq  = ps.getData<std::vector<double> >("th_eq");

  // verify consistency of equilibrium
  std::vector<double> null;
  ps.deriveMhdEq("ALL", null);
  ps.verifyMhdEq(null);

  std::vector<double> elong_eq = ps.getData<std::vector<double> >("elong");

  std::vector<double> p_eq = ps.getData<std::vector<double> >("p_eq");
  std::vector<double> q_eq = ps.getData<std::vector<double> >("q_eq");
  std::vector<double> phit = ps.getData<std::vector<double> >("phit");
  std::vector<double> r_geo = ps.getData<std::vector<double> >("r_geo");
  std::vector<double> z_geo = ps.getData<std::vector<double> >("z_geo");
  std::vector<double> dr_geo_drho = ps.getData<std::vector<double> >("dr_geo_drho");
  std::vector<double> dz_geo_drho = ps.getData<std::vector<double> >("dz_geo_drho");
  std::vector<double> dr_geo_dth = ps.getData<std::vector<double> >("dr_geo_dth");
  std::vector<double> dz_geo_dth = ps.getData<std::vector<double> >("dz_geo_dth");

  const double twoPi = 6.28318530718;
  double thMin = 0.0;
  double thMax = twoPi;
  double dTh = (thMax - thMin) / double(nt1 - 1);
  printf("i     rho      elong    q         gradRho    gradRhoSq    vPrime\n");
  for (size_t i = 0; i < rho_eq.size(); ++i) {
    double sumJ = 0.0;
    double sumGradRho = 0.0;
    double sumGradRhoSq = 0.0;
    for (size_t j = 0; j < th_eq.size() - 1; ++j) {

//       int k = j + nth_eq * i;
      int k = i + ns1 * j;

      double r = r_geo[k];
      double z = z_geo[k];
      double drdr = dr_geo_drho[k];
      double drdt = dr_geo_dth[k];
      double dzdr = dz_geo_drho[k];
      double dzdt = dz_geo_dth[k];

      double det = drdr * dzdt - drdt * dzdr;
      double jac = det * r;
      double gr2   = (dzdt*dzdt + drdt*drdt)/(det*det);

      sumJ += jac;
      sumGradRho += sqrt(gr2);
      sumGradRhoSq += gr2;
    }
    double gradRho = sumGradRho / sumJ;
    double gradRhoSq = sumGradRhoSq / sumJ;
    double vPrime = twoPi * dTh * sumJ;
    
    printf("%3d %8f %8f %8f %10f %10f %10f\n", i, rho_eq[i], elong_eq[i],
	   q_eq[i], gradRho, gradRhoSq, vPrime);
  }

  // testing 2d interpolation
  std::vector<double> rho_fine = linearGrid(0.0, 1.0, 2*(ns1-1)+1);
  // values
  std::vector<double> q_fine = ps.interp1d("q_eq", 0, rho_fine);
  // derivative
  std::vector<double> qPrime_fine = ps.interp1d("q_eq", 1, rho_fine);
  printf("     rho      q      qPrime\n");
  for (size_t i = 0; i < rho_fine.size(); i++) {
    printf("%d %f %f %f\n", i, rho_fine[i], q_fine[i], qPrime_fine[i]);
  }

  // testing 2d interpolation
  std::vector<double> th_fine(2*(ns1-1)+1, 0.0);
  // values
  std::vector<double> r_fine = ps.interp2d("r_geo", 0, 0, rho_fine, th_fine);
  printf("     rho      the     R\n");
  for (size_t i = 0; i < rho_fine.size(); i++) {
    printf("%d %f %f %f\n", i, rho_fine[i], th_fine[i], r_fine[i]);
  }
}

///////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {

  testCxx(argc, argv);

  return 0;
}
