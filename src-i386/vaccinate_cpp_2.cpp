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
List vaccinate_cpp_2(NumericMatrix vac_hist_mat,
                     NumericMatrix v,
                     IntegerMatrix ages_mat,
                     IntegerVector vac_this_year,
                     NumericVector vac_cov,
                     double take,
                     double rho,
                     int vac_strategy){

  int nyears = vac_this_year.size();
  int nindiv = vac_hist_mat.nrow();

  for(int j = 0; j < nyears; ++j){
    NumericVector randnum_vac = runif(nindiv);
      for (int i = 0; i < nindiv; ++i){
        if (vac_this_year[j] == 0){
          vac_hist_mat(i,j) = 0;
          if(j > 0){v(i,j) = v(i,j-1) + 1;}
        } else{
        // determine vaccination probability by incorporating prior vaccination
        if (vac_hist_mat(i,j-vac_strategy) == 1){
          randnum_vac[i] = randnum_vac[i] * (1-rho);
        }
        // vaccinate
        if (randnum_vac[i] <= vac_cov[ages_mat(i,j)] * take){
          vac_hist_mat(i,j) = 1;
          v(i,j) = 0;
        } else {
          vac_hist_mat(i,j) = 0;
          if(j > 0){v(i,j) = v(i,j-1) + 1;}
        }
      }
     }
  }
  List rtn;
  rtn["n"] = nindiv;
  rtn["vac_hist_mat"] = vac_hist_mat;
  rtn["v"] = v;

  return(rtn);
}

/*** R
init_pop <- initialize_pop_cpp(n = 100, nyears = 10, init_ages = sample(1:9,10,replace=TRUE),max_age = 10)
vac_this_year <- c(0,0,0,0,0,1,1,1,1,1)
vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat, ages_mat = init_pop$ages_mat,
                v = init_pop$time_since_last_vac,vac_this_year = vac_this_year,
                vac_cov = c(rep(0.24,10)),take = 1, rho = 0.9,vac_strategy = 1)
*/
