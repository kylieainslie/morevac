#' Vaccine update function
#'
#' This function determines the amount of drift each year.
#' @param years_since_vac_update number of years since last vaccine update
#' @param d euclidean distance threshold for vaccine update
#' @param antigenic_dist antigenic_distance between circulating strain and vaccine strain since last vaccine update
#' @return binary indicator of whether the vaccine will be updated (0 - no, 1 - yes)
#' @keywords morevac
#' @export
vaccine_update <- function(years_since_vac_update, antigenic_dist, d = 0.5){
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
