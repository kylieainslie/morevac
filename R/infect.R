### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort


#' Infection function
#'
#' This function initializes the population before running the model.
#' @param susceptibility Value of an individual's susceptibility.
#' @param foi Number between 0 and 1 indicating the force of infection.
#' @return Indicator of infection (0 = not infected, 1 = infected).
#' @keywords morevac
#' @export
#' @examples
#' infect()
infect <- function(susceptibility = 1, foi = 0.2, randnum_inf = runif(1,0,1)){
  #randnum_inf <- runif(1,0,1)

  if (randnum_inf <= foi*susceptibility) {
    rtn <- 1
  } else {
    rtn <- 0
  }
  return(rtn)
}
