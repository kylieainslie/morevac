#include <Rcpp.h>
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

// Susceptibility function
//
// This function initializes the population before running the model.
// @param inf_history Number years since last infection
// @param vac_history Number of years since last vaccination
// @param ve Vaccine efficacy
// @param drift_x Amount of drift from infection
// @param drift_v Amount of drift from vaccination
// @param version Which susceptibility function to use: 1 = either-or, 2 = multiplicative
// @return Numeric value of susceptibility
// @keywords morevac
// @export
// [[Rcpp::export]]
double suscept_func(int inf_history, int vac_history, double gamma, double drift_x, double drift_v, int version) {

  double vac_ind = 0; // initialize vac_ind
  double rtn = 0;     // initialize return value
// vaccinated in current year
  if(vac_history == 0){
    vac_ind = 1;
  }
// never infected
  if (drift_x > 0 && drift_v > 0){
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
  if (drift_x == 0 && drift_v == 0){
    if (inf_history != 999){
      rtn = 0;
    }
  }

  return(rtn);

}

/*** R
suscept_func(inf_history = 3, vac_history = 0, gamma = 0.4, drift_x = 0.2, drift_v = 0.2, version = 2)
*/
