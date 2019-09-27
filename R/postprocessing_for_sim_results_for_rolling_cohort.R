#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function post processes simulation results for a rolling cohort.
#' @param sim0 array of simulations results (no vac)
#' @param sim1 array of simulations results (annual vac)
#' @param sim2 array of simulation results (biannual vac)
#' @param nsim number of simulations performed (must be the same for each sim case)
#' @param total_year_range vector of years simulations were run over (YYYY format)
#' @param cohort_year_index vector of index of years to get attack rates for
#' @return data frame of attack rates within the cohort by year
#' @keywords morevac
#' @export
postprocess_sim_results_for_rolling_cohort <- function(sim0, sim1, sim2, nsim = 100, total_year_range = 1820:2019, cohort_year_index = 181:200){
  nyears <- length(cohort_year_index)
  sim_ar0 <- matrix(numeric(nyears*nsim),nrow = nyears)
  sim_ar1 <- sim_ar0; sim_ar2 <- sim_ar0
  # subset birth cohort
  for (s in 1:nsim){
    sim_ar0[,s] <- get_cohort_ar(inf_history = sim0$inf_history[,,s],
                                 vac_history = sim0$vac_history[,,s],
                                 ages = sim0$ages[,,s],
                                 years = total_year_range,
                                 year_index = cohort_year_index)$Attack_Rate

    sim_ar1[,s] <- get_cohort_ar(inf_history = sim1$inf_history[,,s],
                                 vac_history = sim1$vac_history[,,s],
                                 ages = sim1$ages[,,s],
                                 years = total_year_range,
                                 year_index = cohort_year_index)$Attack_Rate

    sim_ar2[,s] <- get_cohort_ar(inf_history = sim2$inf_history[,,s],
                                 vac_history = sim2$vac_history[,,s],
                                 ages = sim2$ages[,,s],
                                 years = total_year_range,
                                 year_index = cohort_year_index)$Attack_Rate
  }
  # combine results into one matrix
  sim_ar_all <- rbind(sim_ar0,sim_ar1,sim_ar2)
  vac_strategy <- c(rep("No Vaccination",nyears),rep("Annual",nyears),rep("Every Other Year",nyears))
  years_x3 <- c(rep(total_year_range[cohort_year_index],3))
  # final output dataframe
  rtn <- data.frame(Year = years_x3,
                    Vac_Strategy = vac_strategy,
                    Attack_Rate = apply(sim_ar_all,1,mean),
                    SD_AR = apply(sim_ar_all,1,sd))
  rtn$Lower <- rtn$Attack_Rate - (qnorm(0.975)*rtn$SD_AR/sqrt(nsim))
  rtn$Upper <- rtn$Attack_Rate + (qnorm(0.975)*rtn$SD_AR/sqrt(nsim))
  return(rtn)
}
