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
get_cohort_ar <- function(inf_history, vac_history, ages, years, year_index){
  nyears <- length(year_index)
  cohort = list(); cohort_inf_hist = list(); cohort_vac_hist = list(); cohort_ages = list(); ar_cohort = list()
  ar <- data.frame(Year = years[year_index], Attack_Rate = c(rep(0,nyears)))
  for (j in 1:nyears){
    cohort[[j]] <- which(ages[,year_index[j]] <= current_age)
    cohort_inf_hist[[j]] <- inf_history[cohort[[j]],year_index[j]]
    cohort_vac_hist[[j]] <- vac_history[cohort[[j]],year_index[j]]
    cohort_ages[[j]] <- ages[cohort[[j]],year_index[j]]
    # get attack rate
    ar[j,2] <- sum(cohort_inf_hist[[j]])/length(cohort[[j]])
    # update current age
    current_age <- current_age + 1
  }
  return(ar)
}
