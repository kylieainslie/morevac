### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Run simulations of multiannual() model of infection and vaccination
#' @param sim number of simulations to run
#' @param n number of individuals to be simulated
#' @param years vector of years in YYYY format
#' @param max_age maimum age of an individual before they are removed from the population
#' @param start_vac_year year to start vaccinating (YYYY format)
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
#' @param length_study length of time to follow cohorts
#' @param seed seed for simulations
#' @return list of infection histories, vaccination histories, and age matrices for each simulation
#' @keywords morevac
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom data.table setcolorder
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
                      length_study = 19,
                      seed = NULL){

  # set seed
  if(!is.null(seed)){set.seed(seed)}

  if (vac_strategy == 0){vac_cov <- c(rep(0,length(vac_cov)))}
  nyears <- length(years)
  ### create empty matrix to store sim results
  # histories <- list(inf_history = list(),
  #                      vac_history = list(),
  #                      ages = list())

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

    # post process results to get only childhood cohorts
      my_cohorts <- get_cohorts(inf_history = run_model$inf_history,
                                vac_history = run_model$vac_history,
                                ages = run_model$ages,
                                total_year_range = years)

    # tiny bit of data wrangling
      my_cohorts$inf_history$Sim <- my_cohorts$vac_history$Sim <- s
      my_cohorts$inf_history$Vac_Strategy_num <- my_cohorts$vac_history$Vac_Strategy_num <- vac_strategy
      my_cohorts$inf_history <- setcolorder(my_cohorts$inf_history,
                                            c("Sim","Vac_Strategy_num","Cohort","ID",paste0("Age",0:(length_study-1)))
                                            )
      my_cohorts$vac_history <- setcolorder(my_cohorts$vac_history,
                                            c("Sim","Vac_Strategy_num","Cohort","ID",paste0("Age",0:(length_study-1)))
                                            )

    # remove raw output from memory
      rm(run_model)

    # rbind subsequent simulations
      if (s == 1){
        save_inf_hist <- my_cohorts$inf_history
        save_vac_hist <- my_cohorts$vac_history
      } else {
        save_inf_hist <- rbind(save_inf_hist, my_cohorts$inf_history)
        save_vac_hist <- rbind(save_vac_hist, my_cohorts$vac_history)
      }

      rm(my_cohorts)
      # histories$inf_history[[s]] <- Matrix(run_model$inf_history, sparse = TRUE)
      # histories$vac_history[[s]] <- Matrix(run_model$vac_history, sparse = TRUE)
      # if(!is.null(trim_age_mat)){histories$ages[[s]] <- run_model$ages[,which(years == trim_age_mat):length(years)]
      # } else {histories$ages[[s]] <- run_model$ages}

  }

  # close progress bar
    close(pb)

  # output
    rtn <- list(inf_history = save_inf_hist,
                vac_history = save_vac_hist)


  return(rtn)

}
