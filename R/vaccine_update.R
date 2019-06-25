#' Vaccine update function
#'
#' This function determines the amount of drift each year.
#' @param vac_update_status number of years since last vaccine update
#' @param d distance threshold for vaccine update
#' @param year_counter year of simulation (from 1 to years)
#' @return binary indicator of whether the vaccine will be updated (0 - no, 1 - yes)
#' @keywords morevac
#' @export
vaccine_update <- function(vac_update_status, accumulated_drift, d = 0.5){
  if(vac_update_status == 0){
    rtn <- 0
    return(rtn)
  } else if (vac_update_status > 0){
    if (accumulated_drift > d){
      rtn <- 1
    } else {rtn <- 0}
    return(rtn)
  }
}
