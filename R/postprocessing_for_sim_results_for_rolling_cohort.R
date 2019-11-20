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
postprocess_sim_results_for_rolling_cohort <- function(simdat, nsim = 100, total_year_range = 1820:2028,
                                                       length_study = 19, write.file = FALSE, file = "test"){
  # subset birth cohorts from each sim
  for (s in 1:nsim){
    my_cohorts <- get_cohorts(inf_history = simdat$inf_history[,,s],
                              vac_history = simdat$vac_history[,,s],
                              ages = simdat$ages[,,s],
                              total_year_range = total_year_range)
    my_cohorts$inf_history$Sim <- my_cohorts$vac_history$Sim <- s

    if (s == 1){
      save_inf_hist <- my_cohorts$inf_history
      save_vac_hist <- my_cohorts$vac_history
    } else {
      save_inf_hist <- rbind(save_inf_hist, my_cohorts$inf_history)
      save_vac_hist <- rbind(save_vac_hist, my_cohorts$vac_history)
      }
  }
  if (write.file){
    try(data.table::fwrite(save_inf_hist, file = paste0(file,"_inf_hist.csv"), col.names = TRUE,
                           row.names = FALSE, sep = ","))
    try(data.table::fwrite(save_vac_hist, file = paste0(file,"_vac_hist.csv"), col.names = TRUE,
                           row.names = FALSE, sep = ","))
  }

  rtn <- list(inf_history = save_inf_hist,
              vac_history = save_vac_hist)

  return(rtn)
}

