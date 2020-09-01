#' Vaccine update functions
#'
#' This function determines the amount of drift each year.
#' @param years_since_vac_update number of years since last vaccine update
#' @param d euclidean distance threshold for vaccine update
#' @param antigenic_dist antigenic_distance between circulating strain and vaccine strain since last vaccine update
#' @return binary indicator of whether the vaccine will be updated (0 - no, 1 - yes)
#' @keywords morevac
#' @export
vaccine_update <- function(years_since_vac_update, antigenic_dist, d = 3){
  if(years_since_vac_update == 0){
    rtn <- 0
    return(rtn)
  } else if (years_since_vac_update > 0){
    if (antigenic_dist > d){
      rtn <- 1
    } else {rtn <- 0}
    return(rtn)
  }
}


#' Protection function from Coudeville et al. 2010
#' @param alpha numeric value of alpha parameter. Defaults to log(2.844).
#' @param beta numeric value of beta parameter. Defaults to 1.299.
#' @param log_titre value of individual's log titre.
#' @return numeric value of individual's protection against influenza infection
#' @keywords morevac
#' @export
pi_t_theta <- function(alpha = log(2.844), beta = 1.299, log_titre){
  1 - (1/(1 + exp(beta*(log_titre-alpha))))
}

#' This function determines distance of the vaccine strain from the circulating
#' strain and determines the corresponding vaccine update schedule
#' @param years number of years since last vaccine update
#' @param vac_start_year year that vaccination starts
#' @param antigenic_dist_mat matrix of antigenic distances between circulating strain and vaccine strain
#' @param vac_protect protective effect of vaccine [0,1]
#' @return tibble of vaccine distances, update indicator, and values of gamma
#' @keywords morevac
#' @export
vaccine_distance_func <- function(years = 200, vac_start_year = 181, antigenic_dist_mat, vac_protect = 0.7){
  # create empty vectors
  vaccine_dist <- numeric(years) + 999
  update <- vaccine_dist
  years_since_vac_update <- c(rep(999,vac_start_year-1), rep(0,years-vac_start_year+1))
  mygamma <- numeric(years) + 1
  # initialise counter
  y <- 1

  while (y < years+1){
    if (y < vac_start_year){
      y <- y + 1
      next
    }
    # calculate vaccine distance from circulating strain
    vaccine_dist[y] <- antigenic_dist[y, y-years_since_vac_update[y]]
    # update vaccine?
    update[y] <- vaccine_update(years_since_vac_update = years_since_vac_update[y],
                              antigenic_dist = vaccine_dist[y])
    # change years since vac update to 0 if updated in current year
    years_since_vac_update[y] <- if_else(update[y] == 1, 0, years_since_vac_update[y])
    # determine protective effect of vaccine based on distance from circulating strain
    mygamma[y] <- 1-vac_protect*pi_t_theta(log_titre = log(300) - vaccine_dist[y])
    if(update[y] == 1) {mygamma[y] <- 1-vac_protect}
    # update counters
    if (y < years) {years_since_vac_update[y + 1] <- years_since_vac_update[y] + 1}
    y <- y + 1
  }
  rtn <- tibble(Year = 1:years, Vaccine_Distance = vaccine_dist, Update_Schedule = update,
                Years_Since_Update = years_since_vac_update, Gamma = mygamma)
  return(rtn)
}
