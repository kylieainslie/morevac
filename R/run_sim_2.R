### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Run simulations of multiannual() model of infection and vaccination
#'
#' This function initializes the population before running the model.
#' @param sim number of simulations to run
#' @param nindiv number of individuals to be simulated
#' @param year_range range of years to follow cohort
#' @param age_range range of ages to follow cohort
#' @param vaccov vaccination coverage, should be between 0 and 1 (inclusive)
#' @param filename prefix of outputted figures
#' @param version which vaccination version to use: 1 = either-or, 2 = multiplicative
#' @return two plots 1) plot of annual attack rates for the cohort and 2) a plot of number of lifetime infections by age
#' @keywords morevac
#' @export
run_sim_2 <- function(sim = 100,
                      n = 10000,
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
                      file.out = FALSE,
                      tag = "",
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
      histories$ages[[s]] <- run_model$ages
   }
  close(pb)

  return(histories)

}
