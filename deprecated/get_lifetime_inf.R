### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Get number of lifetime infections by age
#'
#' This function initializes the population before running the model.
#' @param inf_history number of simulations to run
#' @param ages number of individuals to be simulated
#' @param maxage range of years to follow cohort
#' @return matrix of number of lifetime infections for each age. Rownames indicate current age
#' @keywords morevac
#' @export
get_lifetime_inf <- function(inf_history, ages = 0:79, maxage = 80){
  # empty matrix to story lifetime infections
  rtn <- matrix(c(rep(0,maxage^2)),nrow = maxage)
  # use cumsum to find the number of lifetime infections at each age
  inf_dat <- t(apply(inf_history,1,cumsum))
  for (i in ages){
    a <- as.character(ages[i])
    rtn[i,] <- apply(inf_dat[rownames(inf_dat)== a,],2,mean,na.rm=TRUE)
  }
  rownames(rtn) <- as.character(0:79)
  colnames(rtn) <- paste0("Age",0:79)

  return(rtn)
}

