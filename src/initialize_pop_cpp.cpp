#include <Rcpp.h>
#include <algorithm>    // std::fill
using namespace Rcpp;


// [[Rcpp::export]]
List initialize_pop_cpp(int n,int nyears, NumericVector init_ages) {

// initial array
  NumericMatrix inf_hist_mat(n, nyears);
  NumericMatrix vac_hist_mat(n, nyears);
  NumericMatrix suscept_mat(n, nyears);
  NumericMatrix x(n, nyears);
  NumericMatrix v(n, nears);
  NumericMatrix ages_mat(n, nyears)

// create naive matrices for population for first year (0 = not infected, NA = not alive)
    for (int i = 0; i < n; ++i){
      for(int j = 0; j < nyears; ++j){
        if(j <= ages[i]){
          inf_hist_mat(i,j) = 0;   // infection history matrix
          vac_hist_mat(i,j) = 0;   // vaccination history matrix
          suscept_mat(i,j) = 1;    // susceptibility matrix
          x(i,j) = 999;            // time since last infection matrix
          v(i,j) = 999;            // time since last vaccination matrix
          ages_mat(i,j) = j;       // age matrix (the age of an individual in each year)
        } else {
          inf_hist_mat(i,j) = 999;
          vac_hist_mat(i,j) = 999;
          suscept_mat(i,j) = 999;
          x(i,j) = 999;
          v(i,j) = 999;
          ages_mat(i,j) = 999;
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
initialize_pop_cpp(n=10,maxage=10, ages = sample(1:10,10,replace=TRUE))
*/
