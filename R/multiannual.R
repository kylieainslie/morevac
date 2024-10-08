#' Multi-annual model of infection and vaccination
#'
#' This function implements a multi-annual, individual-based, stochastic model of infection and vaccination. The model incorporates three main components: (1) viral evolution, specifically antigenic drift of the infecting virus over time, (2) vaccine kinetics influencing the amount of protection conferred by the vaccine, namely antigenic match of the circulating strain and vaccine strain, waning, and take (defined as the propor- tion of individuals who receive the vaccine and have an immune response), and (3) individual level characteristics, such as age and prior exposure history. All three components are then used to inform an individual’s susceptibility to infection at each time point (here, considered to be one year).
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
#' @return a named list that contains the following elements:
#' - `inf_history`: a named list of elements related to the infection histories of each individual, including
#' - `inf_hist_mat`: a matrix of infection histories, where each row represents each individual and columns represent years. A value of 1 in inf_hist_mat[1,j] indicates that person i had an infection in year j.
#' - `suscept_mat`: a matrix that contains an individuals susceptibility to infection over time. Values range from 0 (completely immune) to 1 (completely susceptible).
#' - `vac_history`: a named list of elements related to the vaccination histories of each individual, including
#' - `n`: number of individuals
#' - `vac_hist_mat`: a matrix of vaccination histories, where each row represents each individual and columns represent years. A value of 1 in vac_hist_mat[1,j] indicates that person i was vaccinated in year j.
#' - `v`: a matrix of the number of years since last vaccination. If never vaccinated the value of v = 999 and then increases each year until vaccination occurs. In the year that an individual was vaccinated v = 0.
#' - `ages`: matrix of each individual's age over time. We assume that an individual dies at age 80 and is then replaced by another person aged 0.
#'  - `drift`: a named list of elements related to viral atigenic drift over time with the following elements:
#'         - `drift`: a data frame with the cumulative amount of drift (drawn from an exponential distribution with rate specified by the user) over time. The data frame has two columns: `x` represents the year and `y` represents the cumulative drift from the first year.
#'         - `antigenic_dist` a matrix with the antigenic distance over time as calculated by `pdist`.
#' - `vac_update`: an identity vector indicating in which years the vaccine formula should be updated (1 = yes, 0 = no).
#' - `gammas`: a vector of the protection conferred by vaccination. In years in which the vaccine formula is a perfect match to the virus strain, then gamma = 0.3. In years in which the vaccination is not a perfect match, the reduction in protection declines until the vaccine strain is updated again.
#' - `vac_this_year`: an identity vector indicating in which years vaccination should occur (1 = yes, 0 = no).
#' @keywords morevac
#' @export
#' @importFrom dplyr if_else
#' @importFrom Rcpp evalCpp
#' @useDynLib morevac, .registration = TRUE
multiannual <- function(n = 10000,
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
    init_pop <- initialize_pop_cpp(
                      n = n,
                      nyears = length(years),
                      init_ages = init_age_vec,
                      max_age = max_age
                      )

  # determine drift
    drift <- drift_func(nyears = length(years), rate = drift_rate)
    antigenic_dist <- drift$antigenic_dist
    if (drift_off){antigenic_dist <- 0}
  # determine vaccine update schedule
    run_update <- vaccine_update_cpp(
                        drift = antigenic_dist,
                        threshold = 4,
                        vac_protect = vac_protect
                        )
  # determine value of protection from infection due to vaccination
  # (the value of protection is dependent on the distance of the
  # vaccination strain relative to the circulating strain)
    gammas <- run_update$gammas
  # determine years of vaccination
    vac_this_year <- if_else(years>=start_vac_year, 1, 0)
  # vaccinate
    vac_pop <- vaccinate_cpp_2(
                     vac_hist_mat = init_pop$vac_hist_mat,
                     v = init_pop$time_since_last_vac,
                     ages_mat = init_pop$ages_mat,
                     vac_this_year = vac_this_year,
                     vac_cov = vac_coverage, take = take,
                     rho = rho,
                     vac_strategy = vac_strategy
                     )

    # calculate delta_v values
      delta_v <- find_delta_v(
                       v = vac_pop$v,
                       dist_mat = antigenic_dist
                       )
    # run infection model
      infect_pop <- infect_cpp_2(
                          inf_history = init_pop$inf_hist_mat,
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
