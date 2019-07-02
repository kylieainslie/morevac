#include <Rcpp.h>
#include <algorithm>    // std::min
using namespace Rcpp;

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
double suscept_func_cpp(int inf_history,
                        int vac_history,
                        double gamma,
                        double drift_x,
                        double drift_v,
                        double wane_rate,
                        int version,
                        int constant) {

  int vac_ind = 0; // initialize vac_ind
  double rtn = 0;  // initialize return value
  double w = 0;       // initialize waning
// vaccinated in current year
  if(vac_history == 0){
     vac_ind = 1;
  }
// determine amount of waning
   w = (1-vac_ind)*((1 - gamma) * vac_history * wane_rate);
// constant drift values
   if(constant == 1){
        if (version == 1){
          rtn = std::min(1.0, std::min(inf_history*drift_x,(vac_ind*gamma)+(vac_history*drift_v) + w)); // either-or
        } else if (version == 2) {
          rtn = std::min(1.0,inf_history*drift_x) * std::min(1.0, gamma + (vac_history*drift_v) + w); // multiplicative
        }
   } else {
// non-constant drift values
     if (version == 1){
       rtn = std::min(1.0, std::min(drift_x,(vac_ind*gamma) + (1-vac_ind)*(gamma + drift_v + w))); // either-or
     } else if (version == 2) {
       rtn = std::min(1.0,drift_x) * std::min(1.0, (vac_ind*gamma) + (1-vac_ind)*(gamma + drift_v + w)); // multiplicative
     }
   }
// infected and drift=0
  if (drift_x == 0 && drift_v == 0 && inf_history < 999){
      rtn = 0;
  }

  return(rtn);
}

/*** R
suscept_func_cpp(inf_history = 0, vac_history = 2,gamma = 0.3,
                 drift_x = 1, drift_v = 0, wane_rate = 0.84,
                 version = 1, constant = 0)
*/
