#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function gets attack rates for a rolling cohort.
#' @param inf_history matrix of infection histories for each person
#' @param vac_history matrix of vaccination history for each person
#' @param ages matrix of ages of each person for every year
#' @param years vector of years to run simulation over (YYYY format)
#' @param year_index vector of index of years to get attack rates for
#' @return data frame of attack rates within the cohort by year
#' @keywords morevac
#' @export
get_cohorts <- function(inf_history, vac_history, ages, enrollment_start_year = 2000, ncohorts = 10, total_year_range, length_study = 18){
  enrollment_stop_year = enrollment_start_year + ncohorts
  year_index <- which(total_year_range %in% enrollment_start_year:enrollment_stop_year)
  cohort = list(); cohort_inf_hist = list(); cohort_vac_hist = list(); cohort_ages = list()
  year_index <- which(total_year_range %in% enrollment_start_year:enrollment_stop_year)
  for (j in 1:ncohorts){
    cohort[[j]] <- which(ages[,year_index[j]] == 0)
    cohort_inf_hist[[j]] <- inf_history[cohort[[j]],year_index[j]:(year_index[j] + length_study)]
    cohort_vac_hist[[j]] <- vac_history[cohort[[j]],year_index[j]:(year_index[j] + length_study)]
    cohort_ages[[j]] <- ages[cohort[[j]],year_index[j]:(year_index[j] + length_study)]
  }
  rtn <- list(cohort_ids = cohort,
              inf_hist = cohort_inf_hist,
              vac_hist = cohort_vac_hist,
              ages = cohort_ages)
  return(rtn)
}
