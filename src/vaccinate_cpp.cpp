#include <Rcpp.h>
using namespace Rcpp;

// Vaccinate function
//
// @param susceptibility Value of an individual's susceptibility.
// @param foi Number between 0 and 1 indicating the force of infection.
// @param randnum_inf Random number between 0 and 1.
// @return Indicator of infection (0 = not infected, 1 = infected).
// @keywords morevac
// @export
// [[Rcpp::export]]
int vaccinate_cpp(int vac_history,
                  int year,
                  double vac_cov,
                  int vac_strategy,
                  int age,
                  double rho,
                  double randnum_vac,
                  int actual_year,
                  int start_vac_year){

  int rtn = 0;

// determine who will be vaccinated
  if (actual_year >= start_vac_year & age >= start_vac_age){

// incorporate prior vaccination
    if (biannual & (year_counter %% 2) != 0){
      if (a > 2 & vac_hist_mat[i,a-2] == 1){rn_vac[i] <- rn_vac[i] * (1-rho)}
    } else {
      if (a > 1 & vac_hist_mat[i,a-1] == 1){rn_vac[i] <- rn_vac[i] * (1-rho)}
    }

// vaccinate
    if (rn_vac[i] <= vac_cov & actual_year >= start_vac_year){
      if (biannual == FALSE) {
        rtn <- 1

      } else if (biannual == TRUE & (year_counter %% 2) != 0){
        rtn <- 1
      } else {rtn <- 0}
    } else {rtn <- 0}
} else {rtn <- 0}

return(rtn)
}

/*** R
vaccinate_cpp(susceptibility = 0.5, foi = 0.15, randnum_inf = 0.05)
*/



