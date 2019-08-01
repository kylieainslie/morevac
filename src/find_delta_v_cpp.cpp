#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Find delta_v function
//
// @param drift vector of dirft values
// @param nyears number of years
// @keywords morevac
// @export
// [[Rcpp::export]]
NumericMatrix find_delta_v(NumericMatrix v, NumericVector drift) {

  int nyears = drift.size();
  int nindiv = v.nrow();
  NumericMatrix delta_v(nindiv,nyears);
  NumericVector tmp_delta(nindiv);

  for(int j = 0; j < nyears; ++j){
    for (int i = 0; i < nindiv; ++i){
      if (v(i,j) >= 999){
        delta_v(i,j) = 1.0;
      } else if (v(i,j) == 0){
        delta_v(i,j) = 0;
        tmp_delta[i] = 0;
      } else {
        tmp_delta[i] += drift[j];
        delta_v(i,j) = std::min(1.0,tmp_delta[i]);
      }
    }
  }

  return(delta_v);
}


/*** R
drift <- drift_func(nyears = 10, rate = 0.5)
init_pop <- initialize_pop_cpp(n = 10, nyears = 10, init_ages = sample(1:9,10,replace=TRUE),max_age = 10)
vac_this_year <- c(0,0,0,0,0,1,0,1,0,1)
vac_pop <- vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat, ages_mat = init_pop$ages_mat,
                v = init_pop$time_since_last_vac,vac_this_year = vac_this_year, vac_cov = 0.5,
                take = 1, rho = 1,
                start_vac_age = 2, stop_vac_age = 5, vac_strategy = 2)
vac_pop$v
delta_v <- find_delta_v(v = vac_pop$v, drift = drift)
delta_v
*/
