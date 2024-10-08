### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Calculate vaccine distance from current strain based on last vaccine update
#'
#' This function determines the amount of drift each year.
#' @param years number of years
#' @param drift vector of drift values
#' @param vac_protect protection from vaccine
#' @return vector of scaled drift values for each year
#' @keywords morevac
#' @export
vac_dist_func <- function(years = 200, drift, vac_protect = 0.7, begin_vac = 181){
# initialize vectors and counters
  vac_dist <- numeric(years)
  update <- vac_dist
  years_since_vac_update <- 0
  j <- 1
  while (j < years+1){
    print(j)
    if(j < begin_vac){
      vac_dist[j] <- 0
      update[j] <- 0
      mygamma[j] <- 1-vac_protect
      j <- j+1
      next
    } else {
      vac_dist[j] <- min(1,sum(drift[(j-years_since_vac_update):j]))
      update[j] <- vaccine_update(years_since_vac_update = years_since_vac_update,
                                accumulated_drift = vaccine_dist[j])
      # change years since vac update to 0 if updated in current year
      years_since_vac_update <- years_since_vac_update * (1 - update[j]) + (1 - update[j])
      # determine protective effect of vaccine based on distance from circulating strain
      if (update[j] == 0){
          mygamma[j] <- (1-vac_protect)*((1/(1-vaccine_dist[j])))
      } else {mygamma[j] <- 1-vac_protect}
      j <- j+1
    }
  }

  return(list(vac_dist = vac_dist,
              update = update,
              gamma = mygamma))
}
