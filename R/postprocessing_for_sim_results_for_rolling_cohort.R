#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function post processes simulation results for a rolling cohort.
#' @param sim_dat list of sparse matrices of simulations results (no vac)
#' @param n_sim number of simulations performed (must be the same for each sim case)
#' @param total_year_range vector of years simulations were run over (YYYY format)
#' @param length_study number of years to get cohorts from
#' @param write.file logical. If TRUE, writes infection and vaccination histories for cohorts to csv file
#' @param file character string indicating the name of the output file. Only used when write.file = TRUE.
#' @return data frame of attack rates within the cohort by year
#' @keywords morevac
#' @export
postprocess_sim_results_for_rolling_cohort <- function(sim_dat, n_sim = 100, total_year_range = 1918:2028,
                                                       length_study = 19, write.file = FALSE, file = "test"){
  # subset birth cohorts from each sim
  for (s in 1:n_sim){
    my_cohorts <- get_cohorts(inf_history = sim_dat$inf_history[[s]],
                              vac_history = sim_dat$vac_history[[s]],
                              ages = sim_dat$ages[[s]],
                              total_year_range = total_year_range)

    my_cohorts$inf_history$Sim <- my_cohorts$vac_history$Sim <- s
    my_cohorts$inf_history <- setcolorder(my_cohorts$inf_history,c("Sim","Cohort","ID",paste0("Age",0:(length_study-1))))
    my_cohorts$vac_history <- setcolorder(my_cohorts$vac_history,c("Sim","Cohort","ID",paste0("Age",0:(length_study-1))))

    # rbind subsequent simulations
    if (s == 1){
      save_inf_hist <- my_cohorts$inf_history
      save_vac_hist <- my_cohorts$vac_history
    } else {
      save_inf_hist <- rbind(save_inf_hist, my_cohorts$inf_history)
      save_vac_hist <- rbind(save_vac_hist, my_cohorts$vac_history)
      }
  }
  # write file to disk
  if (write.file){
    try(data.table::fwrite(save_inf_hist, file = paste0(file,"_inf_hist.csv"), col.names = TRUE,
                           row.names = FALSE, sep = ","))
    try(data.table::fwrite(save_vac_hist, file = paste0(file,"_vac_hist.csv"), col.names = TRUE,
                           row.names = FALSE, sep = ","))
  }
  # output
  rtn <- list(inf_history = save_inf_hist,
              vac_history = save_vac_hist)

  return(rtn)
}

