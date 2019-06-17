#include <Rcpp.h>
using namespace Rcpp;

// Vaccinate function
//
// @param prior_vac Indicator of vaccination in previous year (0=no, 1=yes).
// @param even_year Indicator of whether the current year is an even year (0=no, 1=yes).
// @param vac_cov Number between 0 and 1 (inclusive) indicacting percentage of vaccination coverage.
// @param vac_strategy Vaccination strategy (1=annual, 2=biannual).
// @param age Integer value of age.
// @param rho Number between 0 and 1 (inclusive) indicacting correlation of prior vaccination.
// @param randnum_vac Random number between 0 and 1.
// @param actual_year integer, YYYY year during simulation.
// @param start_vac_year integer of first vaccination year (YYYY)
// @param start_vac_age Integer value of start age of vaccination.
// @return Indicator of vaccination (0 = unvaccinated, 1 = vaccinated).
// @keywords morevac
// @export
// [[Rcpp::export]]
int vaccinate_cpp(int prior_vac,
                  int even_year,
                  double vac_cov,
                  int vac_strategy,
                  int age,
                  double rho,
                  double randnum_vac,
                  int actual_year,
                  int start_vac_year,
                  int start_vac_age){

  int rtn = 0;

// don't vaccinate if it's not a vaccination year or individual is not old enough
  if (actual_year < start_vac_year || age < start_vac_age){
    return(rtn);
  }
// if it's a vaccination year & individual is old enough -> possibly vaccination
  if (actual_year >= start_vac_year && age >= start_vac_age){
// determine vaccination probability by incorporating prior vaccination
    if (prior_vac == 1){
      randnum_vac = randnum_vac * (1-rho);
    }
// vaccinate
  if (randnum_vac <= vac_cov){
      if (vac_strategy == 1) {
        rtn = 1;
      } else if (vac_strategy == 2 && even_year == 1){
        rtn = 1;
      } else {rtn = 0;}
    } else {rtn = 0;}
  } else {rtn = 0;}

return(rtn);
}

/*** R
vaccinate_cpp(prior_vac = 1,
              even_year = 1,
              vac_cov = 0.5,
              vac_strategy = 2,
              age = 5,
              rho = 0,
              randnum_vac = 0.7,
              actual_year = 2008,
              start_vac_year=2000,
              start_vac_age=3)
*/



