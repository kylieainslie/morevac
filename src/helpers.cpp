#include "helpers.h"
#include <algorithm>    // std::min
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
//NumericVector timesTwo(NumericVector x) {
//  return x * 2;
//}

// [[Rcpp::export]]
int suscept_func(int inf_history, int vac_history, double gamma, double drift_x, double drift_v, int version) {

  double vac_ind = 0; // initialize vac_ind
  int rtn;            // initialize return value
// vaccinated in current year
  if(vac_history == 0){
    vac_ind = 1;
  }
// never infected
  if (drift_x > 0 & drift_v > 0){
    if (inf_history == 999){
      if (vac_history > 1/drift_v){ // never vaccinated or vaccinated long enough ago for drift to have diminished protection
        rtn = 1;
      } else if (vac_history <= 1/drift_v){ // vaccinated this year or within last few years
        rtn = (vac_ind*gamma) + (vac_history*drift_v);
      }
    }

// infected and drift>0
    if (inf_history != 999){
      if (vac_history > 1/drift_v){ // never vaccinated
        if (inf_history < 1/drift_x){
          rtn = inf_history*drift_x;
        } else if (inf_history >= 1/drift_x) {
          rtn = 1;
        }
      } else if (vac_history <= 1/drift_v){ // vaccinated
        if (version == 1){
          rtn = std::min(inf_history*drift_x,(vac_ind*gamma)+(vac_history*drift_v)); // either-or
        } else if (version == 2) {
          rtn = inf_history*drift_x * (gamma +(vac_history*drift_v)); // multiplicative
        }
      }
    }
  }
// infected and drift=0
  if (drift_x == 0 & drift_v == 0){
    if (inf_history != 999){
      rtn = 0;
    }
  }

  return(rtn);

}


//NumericMatrix initialize(int nindiv, int maxage, NumericVector ages){

//  float init[nindiv][maxage][6];

//  for (int i=1; i<nindiv; i++)
//    init[i] = 1 / (1+x);

//  return(init);
//}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
