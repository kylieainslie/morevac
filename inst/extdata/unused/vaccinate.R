### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort


#' Vaccination function
#'
#' This function initializes the population before running the model.
#' @param year Year counter.
#' @param vc Vaccine coverage.
#' @param vac_strategy Logical. FALSE = annual, TRUE = biannual.
#' @return Indicator of infection (0 = not infected, 1 = infected).
#' @keywords morevac
#' @export
#' @examples
#' vaccinate()
vaccinate <- function(year = 1, vc = 0.5, vac_strategy = FALSE){

    randnum_vac <- runif(1,0,1)
    # vaccinate
    if (randnum_vac <= vc){
      if (vac_strategy == FALSE) {
        rtn <- 1
      } else if (vac_strategy == TRUE & (year %% 2) != 0){
        rtn <- 1
      } else {rtn <- 0}
    } else {rtn <- 0}
  return(rtn)
}
