/**
 * C++ test for plasma state interface
 * $Id: ccps_test.cxx,v 1.10 2009/01/12 23:57:43 xshare Exp $
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

/**
 * error checking
 */
void checkError(const int lineNumber, const int ier) {
  if (ier != 0) std::cerr << "***Error occurred at line " << 
    lineNumber << '\n';
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
 * Test C interface 
 *
 */
void testC(int argc, char* argv[]) {
  // handle for holding plasma object
  int h[12];

  // error code (0=OK)
  int ier;

  // tag name
  std::string tag = "test";

  int intVal;
  //double doubleVal;

  // set global attributes
  int debug = getDebugLevel(argc, argv);
  psSetDebugLevel(&debug, &ier);
  
  // constructor
  psInit(h, tag.c_str(), &ier, tag.size()); checkError(__LINE__, ier);

  // set number of thermal species
  const int seven = 7;
  psSet_nspec_th(h, &seven, &ier); checkError(__LINE__, ier);
  // check
  psGet_nspec_th(h, &intVal, &ier); checkError(__LINE__, ier);
  assert(intVal == seven);

  // set number of RF minority species
  const int one = 1;
  psSet_nspec_rfmin(h, &one, &ier); checkError(__LINE__, ier);
  // check
  psGet_nspec_rfmin(h, &intVal, &ier); checkError(__LINE__, ier);
  assert(intVal == one);

  // grid
  const int nrho = 101;
  psSet_nrho(h, &nrho, &ier); checkError(__LINE__, ier);
  // check
  psGet_nrho(h, &intVal, &ier); checkError(__LINE__, ier);
  assert(intVal == nrho);

  // grid for runaway electron component
  const int nrho_rw = 26;
  psSet_nrho_rw(h, &nrho_rw, &ier); checkError(__LINE__, ier);
  // check
  psGet_nrho_rw(h, &intVal, &ier); checkError(__LINE__, ier);
  assert(intVal == nrho_rw);
  
  // character size of eqdsk
  int sz;
  psGetSz_eqdsk_file(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " eqdsk_file\n";
  psGetSz_global_label(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " global_label\n";
  psGetSz_simp0_name(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " simp0_name\n";
  psGetSz_newcomb_mapper(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " newcomb_mapper\n";
  psGetSz_tor_mode_label(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " tor_mode_label\n";
  psGetSz_lhrf_src_name(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " lhrf_src_name\n";
  psGetSz_rfmin_name(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " rfmin_name\n";
  psGetSz_runid(h, &sz, &ier);
  std::cout << "CHARACTER*" << sz << " runid\n";

  // destructor
  psFree(h, &ier); checkError(__LINE__, ier);
}

/** 
 * Generate uniform grid
 */

std::vector<double> uniformGrid(int n, double xmin=0.0, double xmax=1.0) {
  std::vector<double> xs(n);
  assert(n > 1);
  double delta = 1.0 / static_cast<double>(n - 1);
  for (size_t i = 0; i < xs.size(); ++i) {
    xs[i] = i * delta;
  }
  return xs;
}

/**
 * Generate profile
 */
std::vector<double> genProf(double f0, double fa, std::vector<double>& rho) {
  std::vector<double> fans( rho.size() );
  for (size_t i = 0; i < rho.size(); ++i) {
    fans[i] = (f0 - fa) * pow( (1.0 - rho[i]*rho[i]) , 2 ) + fa;
  }
  return fans;
}

/** 
 * Test C++ interface
 *
 */
void testCxx(int argc, char* argv[]) {

  int debug = getDebugLevel(argc, argv);
  PlasmaState ps("test", debug);

  // set number of thermal species
  ps.setData("nspec_th", 7);
  int nspec_th = ps.getData<int>("nspec_th");
  assert(nspec_th == 7);
  
  // set number of RF minority species
  ps.setData("nspec_rfmin", 1);
  int nspec_rfmin = ps.getData<int>("nspec_rfmin");
  assert(nspec_rfmin == 1);

  // grid
  ps.setData("nrho", 101);
  int nrho = ps.getData<int>("nrho");
  assert(nrho == nrho);

  // grid for runaway electron component
  ps.setData("nrho_rw", 26);
  int nrho_rw = ps.getData<int>("nrho_rw");
  assert(nrho_rw == 26);

  ps.setData("nicrf_src", 1);
  ps.setData("nrho_icrf", 61);
  ps.setData("nrho_eq", 51);
  ps.setData("nth_eq", 101);
  ps.setData("nr", 0);
  ps.setData("nz", 0);
  std::string version = ps.getVersionId();
  std::string label   = "plasma_state_test ";
  ps.setData("global_label", label + version);
  ps.setData("runid", "Test");
  ps.setData("tokamak_id", "Dummy");
  ps.setData("shot_number", 12345);
  
  double time0 = 0.01;
  ps.setData("t0", time0);
  ps.setData("t1", time0);

  ps.alloc();

  // define thermal species

  ps.setThermalSpecies(-1, -1, 0);         // electrons
  ps.setThermalSpecies(1, 1, 2);           // Deuterium
  int izImp = 6;
  ps.setThermalSpecies(izImp, izImp, 0);   // Carbon+6
  ps.setThermalSpecies(izImp, izImp-1, 0); // Carbon+5
  ps.setThermalSpecies(izImp, izImp-2, 0); // Carbon+4
  izImp = 26;
  ps.setThermalSpecies(izImp, izImp, 0);   // Iron+26
  ps.setThermalSpecies(izImp, izImp-1, 0); // Iron+25
  ps.setThermalSpecies(izImp, izImp-2, 0); // Iron+24

  // define fast species

  ps.setRFMinoritySpecies(1, 1, 1); // Hydrogen

  // combine species lists
  
  ps.finishSpecies();

  // Define antenna list
  std::vector<std::string> names( ps.getData<int>("nicrf_src") );
  names[0] = "Ant_1"; // need to set all names
  ps.setData("icrf_src_name", names);

  // set loop Voltage to 2 everywhere
  ps.setData("v_loop", std::vector<double>(ps.getData<int>("nrho"), 2.0) );

  std::vector<double> vloop = ps.getData<std::vector<double> >("v_loop");
  std::cout << "loop voltage at center: " << vloop[0] << '\n';
  std::cout << "loop voltage at edge  : " << vloop[vloop.size()-1] << '\n';

  std::vector<double> rho_rw   = uniformGrid( ps.getData<int>("nrho_rw") );
  ps.setData("rho_rw", rho_rw);
  std::vector<double> rho_icrf = uniformGrid( ps.getData<int>("nrho_icrf") );
  ps.setData("rho_icrf", rho_icrf);
  std::vector<double> rho      = uniformGrid( ps.getData<int>("nrho") );
  ps.setData("rho", rho);

  // All grids must be defined before state can be written to file
  std::cout << "-- building plasma state equilibrium from g file -- \n";
  ps.updateEquilibrium("g104403.nstx", 0.075001, 0, 0.9);

  std::cout << "nrho_eq = " << ps.getData<int>("nrho_eq") << std::endl;
  std::cout << "nrho_eq_geo = " << ps.getData<int>("nrho_eq_geo") << std::endl;
  
  ps.setData("nrho_eq_geo", 41);
  ps.alloc();
  ps.setData("rho_eq_geo", uniformGrid( ps.getData<int>("nrho_eq_geo") ) );

  std::vector<double> null;
  // by passing a null vector, we're telling plasma state to use its internal grid 
  // to "verify" the consistency of the equilibrium
  ps.verifyMhdEq(null);
  ps.deriveMhdEq("Everything", null);

  bool mdescr_lock = true;
  bool sconfig_lock = true;
  bool simInit_lock = false;
  ps.updateHashCodes(mdescr_lock, sconfig_lock, simInit_lock);
  std::cout << "plasma_state_test: locked machine description (mdescr).\n";
  std::cout << "plasma_state_test: locked shot configuration (sconfig).\n";
  
  ps.writeMachDescr("mdescr_sample_namelist.dat");
  ps.writeShotConfig("sconfig_sample_namelist.dat");

  // read these namelists into a separate state object
  PlasmaState aux("aux", 1);
  aux.readMachDescr("mdescr_sample_namelist.dat", "NEW");
  aux.readShotConfig("sconfig_sample_namelist.dat");

  // store state to file
  aux.store("aux_plasma_state.nc");
  
}

///////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {

  //testC(argc, argv);
  testCxx(argc, argv);

  return 0;
}
