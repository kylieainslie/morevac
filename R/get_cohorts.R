#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function gets attack rates for a rolling cohort.
#' @param inf_history sparse matrix of infection histories for each person
#' @param vac_history sparse matrix of vaccination history for each person
#' @param ages matrix of ages of each person for every year
#' @param years vector of years to run simulation over (YYYY format)
#' @param year_index vector of index of years to get attack rates for
#' @param write.file logical. if true, the infection histories and vaccination
#' histories are written to the working directory
#' @param file character string used as the prefix for the output file if write.file = TRUE
#' @return data frame of attack rates within the cohort by year
#' @keywords morevac
#' @export
get_cohorts <- function(inf_history, vac_history, ages, enrollment_start_year = 2000, ncohorts = 10, total_year_range,
                        length_study = 18, write.file = FALSE, file = "test"){
  enrollment_stop_year = enrollment_start_year + ncohorts
  year_index <- which(total_year_range %in% enrollment_start_year:enrollment_stop_year)
  cohort = list(); cohort_inf_hist = list(); cohort_vac_hist = list(); cohort_ages = list()

  for (j in 1:ncohorts){
    cohort[[j]] <- which(ages[,year_index[j]] == 0)
    cohort_inf_hist[[j]] <- data.frame(ID = 1:length(cohort[[j]]),as.matrix(inf_history[cohort[[j]],year_index[j]:(year_index[j] + length_study)]))
    cohort_vac_hist[[j]] <- data.frame(ID = 1:length(cohort[[j]]), as.matrix(vac_history[cohort[[j]],year_index[j]:(year_index[j] + length_study)]))
    cohort_ages[[j]] <- ages[cohort[[j]],year_index[j]:(year_index[j] + length_study)]
  }

  # bind all cohorts into single data.frame
  save_inf_hist <- rbindlist(cohort_inf_hist, idcol = "Cohort")
  save_vac_hist <- rbindlist(cohort_vac_hist, idcol = "Cohort")
  colnames(save_inf_hist) <- colnames(save_vac_hist) <- c("Cohort", "ID", paste0("Age",0:length_study))

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
