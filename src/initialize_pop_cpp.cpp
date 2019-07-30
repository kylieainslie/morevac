#include <Rcpp.h>
#include <algorithm>    // std::fill
using namespace Rcpp;


// [[Rcpp::export]]
List initialize_pop_cpp(int n,int maxage, NumericVector ages) {

// initial array
  NumericMatrix inf_hist_mat(n, maxage);
  NumericMatrix vac_hist_mat(n, maxage);
  NumericMatrix suscept_mat(n, maxage);
  NumericMatrix x(n, maxage);
  NumericMatrix v(n, maxage);

// create naive matrices for population for first year (0 = not infected, NA = not alive)
    for (int i = 0; i < n; ++i){
      for(int j = 0; j < maxage; ++j){
        if(j <= ages[i]){
          inf_hist_mat(i,j) = 0;   // infection history matrix
          vac_hist_mat(i,j) = 0;   // vaccination history matrix
          suscept_mat(i,j) = 1;    // susceptibility matrix
          x(i,j) = 999;            // time since last infection matrix
          v(i,j) = 999;            // time since last vaccination matrix
        } else {
          inf_hist_mat(i,j) = 999;   // infection history matrix
          vac_hist_mat(i,j) = 999;   // vaccination history matrix
          suscept_mat(i,j) = 999;    // susceptibility matrix
          x(i,j) = 999;            // time since last infection matrix
          v(i,j) = 999;            // time since last vaccination matrix
        }
      }
    }

    List rtn;
    rtn["inf_hist_mat"] = inf_hist_mat;
    rtn["vac_hist_mat"] = vac_hist_mat;
    rtn["suscept_mat"] = suscept_mat;
    rtn["time_since_last_inf"] = x;
    rtn["time_since_last_vac"] = v;

    return(rtn);
}

/*** R
initialize_pop_cpp(n=10,maxage=10, ages = sample(1:10,10,replace=TRUE))
*/
