#include <Rcpp.h>
using namespace Rcpp;

// Calculate lifetime infections
//
// @param a age index
// @param lifetime_inf number of lifetime infection up to age a
// @param inf_stat infection status
// @return number of lifetime infections
// @keywords morevac
// @export
// [[Rcpp::export]]
int lifetime_infections_cpp(int a, int lifetime_inf, int inf_stat){
  int rtn = 0;
  if (a >1){
    rtn = lifetime_inf + inf_stat;
  } else if (a==1){
    rtn = inf_stat;
  }
  return(rtn);
}

/*** R
lifetime_infections_cpp(a = 1, lifetime_inf = 0, inf_stat = 1)
lifetime_infections_cpp(a = 5, lifetime_inf = 1, inf_stat = 1)
*/
