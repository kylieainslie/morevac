#include <Rcpp.h>
using namespace Rcpp;

// Initialize population function
//
// @param n Number of individuals in the population.
// @param nyears Number of years.
// @param init_ages Vector of ages for each individuals.
// @return A list of matrices:
//         1. Infection history matrix
//         2. Vaccination history matrix
//         3. Susceptibility matrix
//         4. Matrix of years since last infection
//         5. Matrix of years since last vaccination
//         6. Age matrix (age of each individual in each year)
// @keywords morevac
// @export
// [[Rcpp::export]]
List initialize_pop_cpp(int n,int nyears, NumericVector init_ages, int max_age) {

// initial array
  NumericMatrix inf_hist_mat(n, nyears);
  NumericMatrix vac_hist_mat(n, nyears);
  NumericMatrix suscept_mat(n, nyears);
  NumericMatrix x(n, nyears);
  NumericMatrix v(n, nyears);
  NumericMatrix ages_mat(n, nyears);

// create naive matrices for population for first year
    for (int i = 0; i < n; ++i){
      for(int j = 0; j < nyears; ++j){

        x(i,j) = 999;            // time since last infection matrix
        v(i,j) = 999;            // time since last vaccination matrix

        if(j == 0){
          inf_hist_mat(i,j) = 0;        // infection history matrix
          vac_hist_mat(i,j) = 0;        // vaccination history matrix
          suscept_mat(i,j) = 1;         // susceptibility matrix
          ages_mat(i,j) = init_ages[i]; // age matrix (the age of an individual in each year)
        } else {
          inf_hist_mat(i,j) = 999;
          vac_hist_mat(i,j) = 999;
          suscept_mat(i,j) = 999;
          if (ages_mat(i,j-1) < max_age - 1){
            ages_mat(i,j) = ages_mat(i,j-1) + 1;
          } else {ages_mat(i,j) = 0;}
         }
      }
    }

    List rtn;
    rtn["inf_hist_mat"] = inf_hist_mat;
    rtn["vac_hist_mat"] = vac_hist_mat;
    rtn["suscept_mat"] = suscept_mat;
    rtn["time_since_last_inf"] = x;
    rtn["time_since_last_vac"] = v;
    rtn["ages_mat"] = ages_mat;

    return(rtn);
}

/*** R
initialize_pop_cpp(n = 10, nyears = 10, init_ages = sample(1:9,10,replace=TRUE),
                   max_age = 10)
*/
