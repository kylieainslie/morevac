### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Drift generating function
#'
#' This function determines the amount of drift each year.
#' @param years number of years to determine drift for
#' @param rate rate of exponential distribution to draw from
#' @return vector of scaled drift values for each year
#' @keywords morevac
#' @export
drift_func <- function(years = 200, rate = 1){
  # drift for infection
  draws <- rexp(years, rate = rate)
  # scale draws by maximum value (to put it on a scale of 0 to 1)
  scaled_drift <- draws/max(draws)
  return(scaled_drift)
}
