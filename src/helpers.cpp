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
double suscept_func_cpp(int inf_history, int vac_history, double gamma, double drift_x, double drift_v, int version) {

  double vac_ind = 0; // initialize vac_ind
  double rtn = 0;     // initialize return value

// vaccinated in current year
  if(vac_history == 0){
    vac_ind = 1;
  }
// never infected
  if (drift_x > 0 && drift_v > 0){
    if (inf_history >= 999){
      if (vac_history > 1/drift_v){ // never vaccinated or vaccinated long enough ago for drift to have diminished protection
        rtn = 1;
      } else if (vac_history <= 1/drift_v){ // vaccinated this year or within last few years
        rtn = (vac_ind*gamma) + (vac_history*drift_v);
      }
    }

// infected and drift>0
    if (inf_history < 999){
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
    if (inf_history < 999){
      rtn = 0;
    }
  }

  return(rtn);

}

/*** R
suscept_func_cpp(inf_history = 3, vac_history = 0, gamma = 0.4, drift_x = 0.2, drift_v = 0.2, version = 2)
*/

// Infection function
//
// This function initializes the population before running the model.
// @param susceptibility Value of an individual's susceptibility.
// @param foi Number between 0 and 1 indicating the force of infection.
// @return Indicator of infection (0 = not infected, 1 = infected).
// @keywords morevac
// @export
// [[Rcpp::export]]
int infect_cpp(double susceptibility, double foi, double randnum_inf){

  int rtn = 0;

  if (randnum_inf <= foi*susceptibility) {
    rtn = 1;
  } else {
    rtn = 0;
  }
  return(rtn);
}

/*** R
infect_cpp(susceptibility = 0.5, foi = 0.15, randnum_inf = 0.51)
*/


// Calculate lifetime infections
//
// @param a age index
// @param lifetime_inf number of lifetime infection up to age a
// @param inf_stat infection status
// @return number of lifetime infections
// @keywords morevac
// @export
// [[Rcpp::export]]
int lifetime_infections_cpp(int a, int lifetime_inf, int inf_stat){
  int rtn = 0;
  if (a >1){
    rtn = lifetime_inf + inf_stat;
  } else if (a==1){
    rtn = inf_stat;
  }
  return(rtn);
}

/*** R
lifetime_infections_cpp(a = 1, lifetime_inf = 0, inf_stat = 1)
lifetime_infections_cpp(a = 5, lifetime_inf = 1, inf_stat = 1)
  */

