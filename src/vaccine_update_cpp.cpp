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
List vaccine_update_cpp(NumericMatrix drift, double threshold, double vac_protect) {

  NumericVector vaccine_dist(drift.nrow());
  IntegerVector update(drift.nrow());
  int years_since_vac_update = 1;
  NumericVector gammas(drift.nrow());
// set initial values of vaccine_dist and update vectors
  vaccine_dist[0] = 0;
  update[0] = 1;
  gammas[0] = 1-vac_protect;
// calculate vaccine distance from circulating strain
  for (int j = 1; j < drift.nrow(); ++j){
//  Rcpp::Rcout<<years_since_vac_update<<std::endl;
    vaccine_dist[j] = drift(j, j - years_since_vac_update);
      if(vaccine_dist[j] > threshold){
        update[j] = 1;              // update vaccine
        years_since_vac_update = 1; // change years since vac update to 0 if updated in current year
//      vaccine_dist = 0;           // reset vaccine_dist to 0
        gammas[j] = 1-vac_protect;  // set protection from vaccination to 1-VE
      } else {
        update[j] = 0;
        years_since_vac_update += 1;
        gammas[j] = 1 - vac_protect*(1-(1/(1 + exp(1.299*(log(200) - vaccine_dist[j]) - log(2.844)))));
      }
  }

  List rtn;
  rtn["distance"] = vaccine_dist;
  rtn["update"] = update;
  rtn["gammas"] = gammas;
  return(rtn);
}

/*** R
drift <- drift_func(nyears = 10, rate = 1)
dist_mat <- drift$antigenic_dist
vaccine_update_cpp(drift = dist_mat, threshold = 4.0, vac_protect = 0.7)
*/
