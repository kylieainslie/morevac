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
                        double wane_rate) {

  int vac_ind = 0; // initialize vac_ind
  double rtn = 0;  // initialize return value
  double w = 0;    // initialize waning
  double pi_x;
  double pi_v;
// vaccinated in current year
  if(vac_history == 0){
     vac_ind = 1;
  }
// determine susceptibility due to infection
  if (inf_history < 999){
    pi_x = (1/(1 + exp(1.299*((std::max(log(200)-drift_x,0)-log(2.844))))));
  } else {pi_x = 1;}
// determine amount of waning
  w = (1-vac_ind)*((1 - gamma) * vac_history * wane_rate);
// determine susceptibility due to infection
  if (vac_history < 999){
    pi_v = (1-gamma)*(1/(1 + exp(1.299*((std::max(log(200)-drift_v,0)-log(2.844)))))) - w;
  } else {pi_v = 1};
// calculate susceptibility
  rtn = pi_x * pi_v;

 return(rtn);
}

/*** R
suscept_func_cpp(inf_history = 0, vac_history = 3,gamma = 0.3,
                 drift_x = 0.7, drift_v = 0.1, wane_rate = 0,
                 version = 1)
*/
