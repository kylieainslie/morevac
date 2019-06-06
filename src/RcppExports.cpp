// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// infect_cpp
int infect_cpp(double susceptibility, double foi, double randnum_inf);
RcppExport SEXP _morevac_infect_cpp(SEXP susceptibilitySEXP, SEXP foiSEXP, SEXP randnum_infSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type susceptibility(susceptibilitySEXP);
    Rcpp::traits::input_parameter< double >::type foi(foiSEXP);
    Rcpp::traits::input_parameter< double >::type randnum_inf(randnum_infSEXP);
    rcpp_result_gen = Rcpp::wrap(infect_cpp(susceptibility, foi, randnum_inf));
    return rcpp_result_gen;
END_RCPP
}
// lifetime_infections_cpp
int lifetime_infections_cpp(int a, int lifetime_inf, int inf_stat);
RcppExport SEXP _morevac_lifetime_infections_cpp(SEXP aSEXP, SEXP lifetime_infSEXP, SEXP inf_statSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type lifetime_inf(lifetime_infSEXP);
    Rcpp::traits::input_parameter< int >::type inf_stat(inf_statSEXP);
    rcpp_result_gen = Rcpp::wrap(lifetime_infections_cpp(a, lifetime_inf, inf_stat));
    return rcpp_result_gen;
END_RCPP
}
// suscept_func_cpp
double suscept_func_cpp(int inf_history, int vac_history, double gamma, double drift_x, double drift_v, int version);
RcppExport SEXP _morevac_suscept_func_cpp(SEXP inf_historySEXP, SEXP vac_historySEXP, SEXP gammaSEXP, SEXP drift_xSEXP, SEXP drift_vSEXP, SEXP versionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type inf_history(inf_historySEXP);
    Rcpp::traits::input_parameter< int >::type vac_history(vac_historySEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type drift_x(drift_xSEXP);
    Rcpp::traits::input_parameter< double >::type drift_v(drift_vSEXP);
    Rcpp::traits::input_parameter< int >::type version(versionSEXP);
    rcpp_result_gen = Rcpp::wrap(suscept_func_cpp(inf_history, vac_history, gamma, drift_x, drift_v, version));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_morevac_infect_cpp", (DL_FUNC) &_morevac_infect_cpp, 3},
    {"_morevac_lifetime_infections_cpp", (DL_FUNC) &_morevac_lifetime_infections_cpp, 3},
    {"_morevac_suscept_func_cpp", (DL_FUNC) &_morevac_suscept_func_cpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_morevac(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
