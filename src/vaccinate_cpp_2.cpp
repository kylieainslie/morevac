#include <Rcpp.h>
using namespace Rcpp;

// Vaccinate function
//
// @param vac_hist_mat
// @param ages_mat
// @param vac_this_year
// @param vac_cov
// @param take
// @param rho Number between 0 and 1 (inclusive) indicacting correlation of prior vaccination.
// @param randnum_vac Vector of random numbers between 0 and 1.
// @param start_vac_age Integer value of start age of vaccination.
// @param vac_strategy Vaccination strategy (e.g., 1=annual, 2=every other year).
// @return Matric of vaccination histories of every individual.
// @keywords morevac
// @export
// [[Rcpp::export]]
NumericMatrix vaccinate_cpp_2(NumericMatrix vac_hist_mat,
                              IntegerMatrix ages_mat,
                              IntegerVector vac_this_year,
                              double vac_cov,
                              double take,
                              double rho,
                              NumericVector randnum_vac,
                              int start_vac_age,
                              int stop_vac_age,
                              int vac_strategy){

  int nyears = vac_this_year.size();
  int nindiv = vac_hist_mat.ncol();

  for(int j = 0; j < nyears; ++j){
      for (int i = 0; i < nindiv; ++i){
        if (vac_this_year[j] == 0){
          vac_hist_mat(i,j) = 0;
        } else{
      // if it's a vaccination year & individual is old enough -> possibly vaccination
        if (ages_mat(i,j) >= start_vac_age){
        // determine vaccination probability by incorporating prior vaccination
          if (vac_hist_mat(i,j)-vac_strategy == 1){
            randnum_vac[i] = randnum_vac[i] * (1-rho);
          }
        // vaccinate
          if (randnum_vac[i] <= vac_cov * take){
            vac_hist_mat(i,j) = 1;
          } else {vac_hist_mat(i,j) = 0;}
        } else {vac_hist_mat(i,j) = 0;}
      }
    }
  }
  return(vac_hist_mat);
}

/*** R
init_pop <- initialize_pop_cpp(n = 10, nyears = 10, init_ages = sample(1:9,10,replace=TRUE),max_age = 10)
vac_this_year <- c(0,0,0,0,0,1,0,1,0,1)
vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat, ages_mat = init_pop$ages_mat, vac_this_year = vac_this_year,
              vac_cov = 0.5, take = 1, rho = 1, randnum_vac = runif(10), start_vac_age = 2, stop_vac_age = 5,
              vac_strategy = 2)
*/



