#include <Rcpp.h>
#include <algorithm>    // std::min
using namespace Rcpp;

// Calculate susceptibility and determine infection status
//
// @param inf_history Number years since last infection
// @param years_since_last_vac Number of years since last vaccination
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
                  NumericMatrix years_since_last_vac,
                  NumericMatrix vac_history,
                  NumericMatrix suscept_mat,
                  NumericMatrix x,
                  NumericMatrix ages_mat,
                  NumericMatrix delta_v,
                  NumericVector gammas,
                  NumericVector drift,
                  NumericVector foi,
                  double wane_rate,
                  double epsilon,
                  int version
                  ) {
// initialize
  int nyears = inf_history.ncol();
  int nindiv = inf_history.nrow();
  double w = 0;                          // initialize waning
  int vac_ind = 0;                       // initialize vac_ind
  double inf_comp = 1.0;
  double vac_comp = 1.0;
  NumericMatrix delta_x(nindiv,nyears);
  NumericVector exposure_count(nindiv);
// begin loop over years and individuals
  for(int j = 0; j < nyears; ++j){
    NumericVector randnum_inf = runif(nindiv);
    for (int i = 0; i < nindiv; ++i){
      if (ages_mat(i,j) == 0) {x(i,j) = 999;} // reset x if age = 0
     // determine delta_x(i,j)
        if (x(i,j) >= 999){
          delta_x(i,j) = 1;
        } else {
          delta_x(i,j) = delta_x(i,j-1) + drift[j];
        }
      // determine infection susceptibility component
        inf_comp = std::min(1.0,delta_x(i,j));

      // determine if individual was vaccinated in current year
        if (years_since_last_vac(i,j) >= 999){
          suscept_mat(i,j) = std::min(1.0,inf_comp);
        } else {
          if(years_since_last_vac(i,j) == 0){
            vac_ind = 1;
            exposure_count[i] += 1;
            vac_comp = std::min(1.0,gammas[j] + (exposure_count[i] * epsilon)); // determine vaccination susceptibility component in vac year
          } else {vac_ind = 0; // non-vac year
            // determine amount of waning
              w = (1.0 - gammas[j-years_since_last_vac(i,j)]) * years_since_last_vac(i,j) * wane_rate;
              vac_comp = std::min(1.0,gammas[j-years_since_last_vac(i,j)] + delta_v(i,j) + w + (exposure_count[i] * epsilon));
            }
          //Rcpp::Rcout<<w<<std::endl; // print output
          // determine version of susceptibility
            if (version == 1){
              suscept_mat(i,j) = std::min(1.0, std::min(inf_comp, vac_comp)); // either-or
            } else if (version == 2) {
              suscept_mat(i,j) = std::min(1.0,inf_comp * vac_comp); // multiplicative
            } // else if (version == 3) {
              //suscept_mat(i,j) = std::min(1.0,std::min(1.0,delta_x(i,j))*std::min(1.0, 1.0 - exp((-wane_rate*years_since_last_vac(i,j))*(1-delta_v(i,j))))) // exponential waning
            //}
          }
      // determine infection status
        if (randnum_inf[i] <= foi[j]*suscept_mat(i,j)) {
          inf_history(i,j) = 1;
          x(i,j) = 0;
          suscept_mat(i,j) = 0 + (exposure_count[i] * epsilon); // increase in minimum susceptibility after every exposure
          delta_x(i,j) = 0 + (exposure_count[i] * epsilon);
          exposure_count[i] += 1;
        } else {inf_history(i,j) = 0;}
      // update x(i,j+1)
        if (j < nyears - 1){
          if (ages_mat(i,j+1) > 0){
              x(i,j+1) = x(i,j) + 1;
          } else {exposure_count[i] = 0;}
        }
      }
    }
  List rtn;
  rtn["inf_hist_mat"] = inf_history;
  rtn["suscept_mat"] = suscept_mat;
  rtn["x"] = x;
  rtn["delta_x"] = delta_x;
  rtn["num_exposures"] = exposure_count;
  return(rtn);
}

/*** R
# input parameter values
 nyears <- 209
 maxage <- 80
 nindiv <- 10000
 betas <- c(0.4, rep(0.2,nyears-1))
 # initialize population
 init_age_vec <- sample(0:maxage-1,nindiv,replace=TRUE)
 init_pop <- initialize_pop_cpp(n = nindiv, nyears = nyears, init_ages = init_age_vec, max_age = maxage)
 vac_this_year <- c(rep(0,nyears/2),rep(1,nyears/2))
 drift <- drift_func(nyears = nyears, rate = 0.25)
 # determine vaccine update schedule
 run_update <- vaccine_update_cpp(drift = drift, threshold = 0.5, vac_protect = 0.7)
 gammas <- run_update$gammas
 vac <- vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat,
                        ages_mat = init_pop$ages_mat,
                        v = init_pop$time_since_last_vac,
                        vac_this_year = vac_this_year,
                        vac_cov = c(rep(1,maxage)),
                        take = 1,
                        rho = 1,
                        vac_strategy = 2)
 delta_v <- find_delta_v(v = vac$v, drift = drift)
 infect_pop <- infect_cpp_2(inf_history = init_pop$inf_hist_mat,
                            years_since_last_vac = vac$v,
                            vac_history = vac$vac_hist_mat,
                            suscept_mat = init_pop$suscept_mat,
                            x = init_pop$time_since_last_inf,
                            ages_mat = init_pop$ages_mat,
                            drift = drift,
                            delta_v = delta_v,
                            gammas = gammas,
                            foi = betas,
                            wane_rate = 0,
                            epsilon = 0.03,
                            version = 2)
 person <- data.frame(Year = 1820:(1820+nyears-1),
                      Vac_History = vac$vac_hist_mat[2,],
                      Inf_History = infect_pop$inf_hist_mat[2,],
                      X = infect_pop$x[2,],
                      Susceptibility = infect_pop$suscept_mat[2,],
                      Drift = drift,
                      Update = run_update$update)
*/
