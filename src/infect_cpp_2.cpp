#include <Rcpp.h>
#include <algorithm>    // std::min
using namespace Rcpp;

// Calculate susceptibility and determine infection status
//
// @param inf_history Number years since last infection
// @param vac_history Number of years since last vaccination
// @param suscept_mat
// @param x
// @param ages_mat
// @param delta_v
// @param gammas
// @param drift
// @param foi
// @param wane_rate
// @param version Which susceptibility function to use: 1 = either-or, 2 = multiplicative
// @return Numeric value of susceptibility
// @keywords morevac
// @export
// [[Rcpp::export]]
List infect_cpp_2(NumericMatrix inf_history,
                        NumericMatrix vac_history,
                        NumericMatrix suscept_mat,
                        NumericMatrix x,
                        NumericMatrix ages_mat,
                        NumericMatrix delta_v,
                        NumericVector gammas,
                        NumericVector drift,
                        NumericVector foi,
                        double wane_rate,
                        int version
                        ) {

  int nyears = inf_history.ncol();
  int nindiv = inf_history.nrow();
  double w = 0;                          // initialize waning
  int vac_ind = 0;                       // initialize vac_ind
  NumericMatrix delta_x(nindiv,nyears);
  NumericVector tmp_delta(nindiv);

  for(int j = 0; j < nyears; ++j){
    NumericVector randnum_inf = runif(nindiv);
    for (int i = 0; i < nindiv; ++i){
     // determine delta_x
        if (x(i,j) >= 999){
          delta_x(i,j) = 1;
        } else if (x(i,j) == 0){
          delta_x(i,j) = 0;
          tmp_delta[i] = 0;
        } else {
          tmp_delta[i] += drift[j];
          delta_x(i,j) = std::min(1.0,tmp_delta[i]);
        }
      // determine susceptibility
        if (ages_mat(i,j) == 0 || x(i,j)>=999) {suscept_mat(i,j) = 1;
        } else if (x(i,j) == 0){
          suscept_mat(i,j) = 0;
        } else {
        // determine if individual was vaccinated in current year
          if(vac_history(i,j) == 1){
            vac_ind = 1;
          } else {vac_ind = 0;}
        // determine amount of waning
          w = (1-vac_ind)*((1 - gammas[j]) * vac_history(i,j) * wane_rate);
          if (vac_history(i,j) < 999){
            if (version == 1){
              suscept_mat(i,j) = std::min(1.0, std::min(delta_x(i,j),(vac_ind*gammas[j]) + (1-vac_ind)*(gammas[j] + delta_v(i,j) + w))); // either-or
            } else if (version == 2) {
              suscept_mat(i,j) = std::min(1.0,delta_x(i,j)) * std::min(1.0, (vac_ind*gammas[j]) + (1-vac_ind)*(gammas[j] + delta_v(i,j) + w)); // multiplicative
            }
          } else if (vac_history(i,j) >= 999){
            suscept_mat(i,j) = std::min(1.0,delta_x(i,j));
          }
        }
      // determine infection status
        if (randnum_inf[i] <= foi[j]*suscept_mat(i,j)) {
          inf_history(i,j) = 1;
          x(i,j) = 0;
          suscept_mat(i,j) = 0;
        } else {inf_history(i,j) = 0;}
      // update x(i,j+1)
        if (j < nyears - 1){
          if (ages_mat(i,j+1) > 0){
              x(i,j+1) = x(i,j) + 1;
          }
        }
      }
    }
  List rtn;
  rtn["inf_hist_mat"] = inf_history;
  rtn["suscept_mat"] = suscept_mat;
  rtn["x"] = x;
  rtn["delta_x"] = delta_x;
  return(rtn);
}

/*** R
# input parameter values
nyears <- 200
maxage <- 80
nindiv <- 1000
betas <- c(0.4, rep(0.2,nyears-1))
# initialize population
init_age_vec <- sample(0:maxage-1,nindiv,replace=TRUE)
init_pop <- initialize_pop_cpp(n = nindiv, nyears = nyears, init_ages = init_age_vec, max_age = maxage)
vac_this_year <- c(rep(0,nyears))
drift <- drift_func(nyears = nyears, rate = 0.5)
# determine vaccine update schedule
run_update <- vaccine_update_cpp(drift = drift, threshold = 0.5, vac_protect = 0.7)
gammas <- run_update$gammas
vac <- vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat, ages_mat = init_pop$ages_mat,
                       v = init_pop$time_since_last_vac,vac_this_year = vac_this_year, vac_cov = 0.5,
                       take = 1, rho = 1, start_vac_age = 2, stop_vac_age = 5, vac_strategy = 2)
delta_v <- find_delta_v(v = vac$v, drift = drift)
infect_pop <- infect_cpp_2(inf_history = init_pop$inf_hist_mat,
                           vac_history = vac$vac_hist_mat,
                           suscept_mat = init_pop$suscept_mat,
                           x = init_pop$time_since_last_inf,
                           ages_mat = init_pop$ages_mat,
                           drift = drift,
                           delta_v = delta_v,
                           gammas = gammas,
                           foi = betas,
                           wane_rate = 0,
                           version = 2)
*/
