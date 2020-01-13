### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Drift generating function
#'
#' This function determines the amount of drift each year.
#' @param nyears number of years to determine drift for
#' @param rate rate of exponential distribution to draw from
#' @return distance matrix
#' @keywords morevac
#' @export
drift_func <- function(nyears = 200, rate = 1){
  # drift for infection
  draws <- rexp(nyears, rate = rate)
  drift <- cumsum(draws)
  drift_data <- data.frame(x = 1:nyears, y = drift)
  antigenic_dist <- pdist(drift_data) # calculate distance between strains
  return(antigenic_dist)
}
