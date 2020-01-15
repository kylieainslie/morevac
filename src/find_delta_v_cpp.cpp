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
NumericMatrix find_delta_v(NumericMatrix v, NumericMatrix dist_mat) {

  int nyears = dist_mat.nrow();
  int nindiv = v.nrow();
  NumericMatrix delta_v(nindiv,nyears);

  for(int j = 0; j < nyears; ++j){
    for (int i = 0; i < nindiv; ++i){
      if (v(i,j) >= 999){
        delta_v(i,j) = 999;
      } else {
        delta_v(i,j) = dist_mat(j-v(i,j), j);
      }
    }
  }

  return(delta_v);
}


/*** R
drift <- drift_func(nyears = 10, rate = 1)
antigenic_dist <- drift$antigenic_dist
init_pop <- initialize_pop_cpp(n = 10, nyears = 10, init_ages = sample(1:9,10,replace=TRUE),max_age = 10)
vac_this_year <- c(0,0,0,0,0,1,1,1,1,1)
vac_pop <- vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat, ages_mat = init_pop$ages_mat,
                v = init_pop$time_since_last_vac,vac_this_year = vac_this_year, vac_cov = c(rep(0.44,10)),
                take = 1, rho = 0.9, vac_strategy = 1)
vac_pop$v
delta_v <- find_delta_v(v = vac_pop$v, dist_mat = antigenic_dist)
delta_v
*/
