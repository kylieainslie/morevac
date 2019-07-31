### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function initializes the population before running the model.
#' @param n number of individuals to be simulated
#' @param years number of years to run simulation over
#' @param maxage maximum age of an individual (removed from population after maxage)
#' @param start_year year to start simulation
#' @param vac_start_year year that vaccination starts
#' @param start_vac_age age at which an individual may be vaccinated
#' @param stop_vac_age age at which vaccination stops
#' @param vac_coverage vaccination coverage
#' @param beta_pandemic force of infection when simulation starts
#' @param beta_epidemic annual force of infection (after initialization of model)
#' @param delta_x drift parameter from natural infection
#' @param delta_v drift parameter from vaccination
#' @param mygamma protective effect of vaccine
#' @param suscept_func_version integer indicating which susceptibility version to use.
#'        1 = either-or, 2 = multiplicative.
#' @param vac_strategy Integer value indicating frequency of vaccination (1 = annual, 2 = biannual, 3 =triannual,...)
#' @param rho correlation of vaccination
#' @param wane amount of protection of vaccine due to waning (0, 1) (inclusive)
#' @param take percentage of vaccine take (0, 1) (inclusive)
#' @param seed set random seed
#' @return list with two elements: 1) a list of infection histories and attack rates and
#'         2) a plot of annual attack rates by vaccination scenario
#' @keywords morevac
#' @export
multiannual <- function(n = 1000,
                         years = 1820:2019,
                         max_age = 80,
                         start_vac_year = 2000,
                         start_vac_age = 2,
                         stop_vac_age = 10,
                         vac_coverage = 0.5,
                         beta_pandemic = 0.4,
                         beta_epidemic = 0.2,
                         delta_x = NULL,
                         delta_v = NULL,
                         vac_protect = 0.7,
                         suscept_func_version = 1,
                         vac_strategy = 1,
                         rho = 0,
                         wane = 0,
                         take = 1,
                         seed = NULL
){

  # set seed
    if (!is.null(seed)){set.seed(seed)}
  # initialize the population
    init_age_vec <- sample(0:maxage-1,n,replace = TRUE)
    init_pop <- initialize_pop_cpp(n = n, nyears = length(years),
                                   init_ages = init_ages_vec, max_age = max_age)

  # determine drift
    drift <- drift_func(nyears = length(years), rate = 0.5)
  # determine vaccine update schedule
    run_update <- vaccine_update_cpp(drift = drift, threshold = 0.5, vac_protect = vac_protect)
    vac_update <- run_update$update
  # determine value of protection from infection due to vaccination
  # (the value of protection is dependent on the distance of the
  # vaccination strain relative to the circulating strain)
    gammas <- run_update$gammas
  # determine years of vaccination (based on vac_strategy)
    vac_this_year <- ifelse(years>=start_vac_year & years %% vac_strategy == 0, 1, 0)
    vac_this_year <- ifelse(is.na(vac_this_year),0,vac_this_year)
  # generate random numbers for infection and vaccination
    rn_inf <- runif(n,0,1)
    rn_vac <- runif(n,0,1)
  # vaccinate
    vac_hist_mat <- vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat,
                                    ages_mat = init_pop$ages_mat,
                                    vac_this_year = vac_this_year,
                                    vac_cov = 0.5, take = 1, rho = 1,
                                    randnum_vac = rn_vac,
                                    start_vac_age = start_vac_age,
                                    stop_vac_age = stop_vac_age,
                                    vac_strategy = vac_strategy)




}
