#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Vaccine update function
//
// @param drift vector of dirft values
// @param nyears number of years
// @keywords morevac
// @export
// [[Rcpp::export]]
List vaccine_update_cpp(NumericVector drift,double threshold, double vac_protect) {

  double vaccine_dist;
  IntegerVector update(drift.size());
  int years_since_vac_update = 0;
  NumericVector gammas(drift.size());
// set initial values of vaccine_dist and update vectoes
  vaccine_dist = 0;
  update[0] = 1;
  gammas[0] = 1-vac_protect;
// calculate vaccine distance from circulating strain
  for (int j = 1; j < drift.size(); ++j){
    vaccine_dist += drift[j];
    if(vaccine_dist > threshold){
      update[j] = 1;              // update vaccine
      years_since_vac_update = 0; // change years since vac update to 0 if updated in current year
      vaccine_dist = 0;           // reset vaccine_dist to 0
      gammas[j] = 1-vac_protect;  // set protection from vaccination to 1-VE
      } else {
      update[j] = 0;
      years_since_vac_update += 1;
      gammas[j] = (1-vac_protect)*((1/(1-vaccine_dist)));
    }
  }

  List rtn;
  rtn["update"] = update;
  rtn["gammas"] = gammas;
  return(rtn);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
vaccine_update_cpp(drift = drift_vec, threshold = 0.5, vac_protect = 0.7)
*/
