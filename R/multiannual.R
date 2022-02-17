### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function initializes the population before running the model.
#' @param n number of individuals to be simulated
#' @param years vector of years to run simulation over (YYYY format)
#' @param max_age maximum age of an individual (removed from population after max_age)
#' @param start_vac_year year that vaccination starts (YYYY)
#' @param vac_coverage vaccination coverage
#' @param betas vector of force of infection parameters for every year
#' @param vac_protect protective effect of vaccine
#' @param vac_strategy Integer value indicating frequency of vaccination (1 = annual, 2 = biannual, 3 =triannual,...)
#' @param rho correlation of vaccination
#' @param wane amount of protection of vaccine due to waning (0, 1) (inclusive)
#' @param take percentage of vaccine take (0, 1) (inclusive)
#' @param drift_rate rate of exponential drift: exp(drift_rate) used to calculate amount of drift each year
#' @param drift_off logical. if TRUE there is no antigenic drift
#' @param epsilon exposure penalty. Takes values between 0 and 1.
#' @param seed set random seed
#' @return list with two elements: 1) a list of infection histories and attack rates and
#'         2) a plot of annual attack rates by vaccination scenario
#' @keywords morevac
#' @importFrom dplyr if_else
#' @export
multiannual <- function(n = 30000,
                        years = 1918:2028,
                        max_age = 80,
                        start_vac_year = 2000,
                        vac_coverage = c(rep(0.5,80)),
                        betas = c(0.4,rep(0.2,110)),
                        vac_protect = 0.7,
                        vac_strategy = 1,
                        rho = 0.9,
                        wane = 0,
                        take = 1,
                        drift_rate = 1,
                        drift_off = FALSE,
                        epsilon = 0,
                        seed = NULL
                        ){

  # set seed
    if (!is.null(seed)){set.seed(seed)}
  # initialize the population
    init_age_vec <- sample(1:max_age-1,n,replace = TRUE)
    init_pop <- initialize_pop_cpp(n = n, nyears = length(years), init_ages = init_age_vec, max_age = max_age)

  # determine drift
    drift <- drift_func(nyears = length(years), rate = drift_rate)
    antigenic_dist <- drift$antigenic_dist
    if (drift_off){antigenic_dist <- 0}
  # determine vaccine update schedule
    run_update <- vaccine_update_cpp(drift = antigenic_dist, threshold = 4, vac_protect = vac_protect)
  # determine value of protection from infection due to vaccination
  # (the value of protection is dependent on the distance of the
  # vaccination strain relative to the circulating strain)
    gammas <- run_update$gammas
  # determine years of vaccination
    vac_this_year <- if_else(years>=start_vac_year, 1, 0)
  # vaccinate
    vac_pop <- vaccinate_cpp_2(vac_hist_mat = init_pop$vac_hist_mat,
                               v = init_pop$time_since_last_vac,
                               ages_mat = init_pop$ages_mat,
                               vac_this_year = vac_this_year,
                               vac_cov = vac_coverage, take = take,
                               rho = rho, vac_strategy = vac_strategy
                               )

  # calculate delta_v values
    delta_v <- find_delta_v(v = vac_pop$v, dist_mat = antigenic_dist)
  # run infection model
    infect_pop <- infect_cpp_2(inf_history = init_pop$inf_hist_mat,
                               vac_history = vac_pop$vac_hist_mat,
                               years_since_last_vac = vac_pop$v,
                               suscept_mat = init_pop$suscept_mat,
                               x = init_pop$time_since_last_inf,
                               ages_mat = init_pop$ages_mat,
                               dist_mat = antigenic_dist, delta_v = delta_v,
                               gammas = gammas, foi = betas,
                               wane_rate = wane,
                               epsilon = epsilon
                               )

  # return
    rtn <- list(inf_history = infect_pop$inf_hist_mat,
                vac_history = vac_pop$vac_hist_mat,
                suscept_mat = infect_pop$suscept_mat,
                ages = init_pop$ages_mat,
                drift = drift,
                vac_update = run_update$update,
                distance = run_update$distance
                #gammas = gammas,
                #vac_this_year = vac_this_year
      )

    # remove objects from memory
      rm(init_age_vec)
      rm(init_pop)
      rm(vac_pop)
      rm(delta_v)
      rm(infect_pop)

      # # add column names
      # for(i in seq_along(rtn)) {
      #   if(is.matrix(rtn[[i]]) & dim(rtn[[i]])[2] == length(years)){
      #     colnames(rtn[[i]]) <- letters[1:10]
      #   }
      # }

    # output
      return(rtn)

}
