#' Vaccine update function
#'
#' This function determines the amount of drift each year.
#' @param years_since_vac_update number of years since last vaccine update
#' @param d distance threshold for vaccine update
#' @param accumulated_drift amount of drift since last vac update
#' @return binary indicator of whether the vaccine will be updated (0 - no, 1 - yes)
#' @keywords morevac
#' @export
vaccine_update <- function(years_since_vac_update, accumulated_drift, d = 0.5){
  if(years_since_vac_update == 0){
    rtn <- 0
    return(rtn)
  } else if (years_since_vac_update > 0){
    if (accumulated_drift > d){
      rtn <- 1
    } else {rtn <- 0}
    return(rtn)
  }
}
