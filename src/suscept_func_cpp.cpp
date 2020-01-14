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
  double titre_inf;
  double titre_vac;
  double epsilon = 0.0001;
// vaccinated in current year
  if(vac_history == 0){vac_ind = 1;}
// determine susceptibility due to infection
  if (inf_history < 999){
    // determine log titre for value due to infection
    titre_inf = log(200)-drift_x;
    pi_x = (1/(1 + exp(1.299*((titre_inf-log(2.844))))));
  } else {pi_x = 1.0;}
  //  Rcpp::Rcout << "pi_x = " << std::endl;
  //  Rcpp::Rcout << pi_x << std::endl;
// determine susceptibility due to infection
  if (vac_history < 999){
    // determine amount of waning
    w = gamma + (1-gamma)*wane_rate*(1-vac_ind) - epsilon;
  //  Rcpp::Rcout << "w = " << std::endl;
  //  Rcpp::Rcout << w << std::endl;
    // determine log titre for based on vaccination
    titre_vac = log(1/w - 1)/1.299 + log(2.844) - drift_v;
  //  Rcpp::Rcout << "titre_vac = " << std::endl;
  //  Rcpp::Rcout << titre_vac << std::endl;
    pi_v = (1/(1 + exp(1.299*((titre_vac-log(2.844))))));
  //  Rcpp::Rcout << "pi_v = " << std::endl;
  //  Rcpp::Rcout << pi_v << std::endl;
  } else {pi_v = 1.0;}
// calculate susceptibility
  rtn = pi_x * pi_v;

 return(rtn);
}

/*** R
suscept_func_cpp(inf_history = 1, vac_history = 1, gamma = 0.3,
                 drift_x = 15, drift_v = 0, wane_rate = 1)
*/
