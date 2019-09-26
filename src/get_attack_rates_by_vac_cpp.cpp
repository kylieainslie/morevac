#include <Rcpp.h>
using namespace Rcpp;

// Multi-annual model of infection and vaccination (version 2)
//
// This function initializes the population before running the model.
// @param inf_history matrix of infection histories for each person
// @param ages_mat matrix of ages of each person for every year
// @param vac_history matrix of vaccination history for each person
// @param years vector of years to run simulation over (YYYY format)
// @return data frame or list of data frames with attack rates
// @keywords morevac
// @export
// [[Rcpp::export]]
get_attack_rates_by_vac_cpp(NumericMatrix inf_history, NumericMatrix vac_history, NumericVector years){

  int nyears = inf_history.ncol();
  int nindiv = inf_history.nrow();
  NumericVector ar_vac(nyears);
  NumericVector ar_unvac(nyears);

// number of individuals vaccinated in each year
    for(int j = 0; j < nyears; ++j){
      vac_indivs <- which(vac_history[,j]==1)
      ar_vac[j] <- ifelse(length(vac_indivs) == 0, 0, sum(inf_history[vac_indivs,j])/length(vac_indivs))
      ar_unvac[j] <- ifelse(length(vac_indivs) == 0, sum(inf_history[,j])/n, sum(inf_history[-vac_indivs,j])/(n - length(vac_indivs)))
    }
    ar_vac <- ifelse(is.nan(ar_vac),0,ar_vac)
      ar_by_vac_dat <- data.frame(Year = rep(years,2), Vac_Status = c(rep("Vaccinated",nyears),rep("Unvaccinated",nyears)),
                                  Attack_Rate = c(ar_vac,ar_unvac))
      ar_by_vac_dat <- ar_by_vac_dat[order(ar_by_vac_dat$Year),]

  return(rtn)
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
