#include <Rcpp.h>
using namespace Rcpp;

// Infection function
//
// This function initializes the population before running the model.
// @param susceptibility Value of an individual's susceptibility.
// @param foi Number between 0 and 1 indicating the force of infection.
// @param randnum_inf Random number between 0 and 1.
// @return Indicator of infection (0 = not infected, 1 = infected).
// @keywords morevac
// @export
// [[Rcpp::export]]
int infect_cpp(double susceptibility, double foi, double randnum_inf){

  int rtn = 0;

  if (randnum_inf <= foi*susceptibility) {
    rtn = 1;
  } else {
    rtn = 0;
  }
  return(rtn);
}

/*** R
infect_cpp(susceptibility = 0.5, foi = 0.15, randnum_inf = 0.05)
*/
