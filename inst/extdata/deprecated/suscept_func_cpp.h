#include <Rcpp.h>
using namespace Rcpp;


#ifndef MIN
#define MIN(a,b) ((a) > (b) ? (b) : (a)) // define MIN function for use later
#endif