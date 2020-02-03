### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Run simulations of multiannual() model of infection and vaccination
#' @param sim number of simulations to run
#' @param n number of individuals to be simulated
#' @param years vector of years in YYYY format
#' @param max_age maimum age of an individual before they are removed from the population
#' @param vac_cov vector of vaccination coverages for each age group (should be of length max_age)
#' @param betas vector of force of infetion for each year (should be same length as years)
#' @param vac_protect vaccine efficacy
#' @param vac_strategy integer indicating which vaccination strategy to use (0 = no vaccination, 1 = annual, 2 = biennial)
#' @param rho correlation of repeat vaccination in individuals (between 0 and 1 (inclusive))
#' @param wane amount of vaccine waning
#' @param take proportion of individuals who receive the vaccine and are protected
#' @param epsilon exposure penalty
#' @param drift_off logical. if TRUE there will be no antigenic drift
#' @param trim_age_mat year before which age matrix is subsetted. If the year 2000 is used, then age matrix will only contain ages for year 2000 and beyond.
#' @param seed seed for simulations
#' @return list of infection histories, vaccination histories, and age matrices for each simulation
#' @keywords morevac
#' @export
run_sim_2 <- function(sim = 100,
                      n = 30000,
                      years = 1918:2028,
                      max_age = 80,
                      start_vac_year = 2000,
                      vac_cov = c(rep(0.5,80)),
                      betas = c(0.4,rep(0.2,110)),
                      vac_protect = 0.7,
                      vac_strategy = 1,
                      rho = 0.9,
                      wane = 0,
                      take = 1,
                      epsilon = 0.03,
                      drift_off = FALSE,
                      trim_age_mat = NULL,
                      seed = NULL){

  if (vac_strategy == 0){vac_cov <- c(rep(0,length(vac_cov)))}
  nyears <- length(years)
  ### create empty matrix to store sim results
  histories <- list(inf_history = list(),
                       vac_history = list(),
                       ages = list())

  ### create progress bar
  pb <- txtProgressBar(min = 0, max = sim, style = 3)
  ### start simulations
  for (s in 1:sim){
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, s)
    # run model
      run_model <- multiannual(n = n, years = years, max_age = max_age,
                               start_vac_year = start_vac_year,
                               vac_coverage = vac_cov, betas = betas,
                               vac_protect = vac_protect,
                               vac_strategy = vac_strategy,  rho = rho,
                               wane = wane, take = take, epsilon = epsilon,
                               drift_off = drift_off)

      histories$inf_history[[s]] <- Matrix(run_model$inf_history$inf_hist_mat, sparse = TRUE)
      histories$vac_history[[s]] <- Matrix(run_model$vac_history$vac_hist_mat, sparse = TRUE)
      if(!is.null(trim_age_mat)){histories$ages[[s]] <- run_model$ages[,which(years == trim_age_mat):length(years)]
      } else {histories$ages[[s]] <- run_model$ages}

   }
  close(pb)

  return(histories)

}
