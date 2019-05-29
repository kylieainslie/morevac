### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort


#' Initialize population
#'
#' This function initializes the population before running the model.
#' @param nindiv Number of individuals in the simulated population. Defaults to 1000.
#' @param maxage Maximum age of individuals after which they die and are removed from the population. Defaults to 80.
#' @return List containing initialized infection history matrix, vaccination history matrix, susceptibility matrix, attack rate vector, and incidence vector.
#' @keywords morevac
#' @export
#' @examples
#' initialize_pop()
initialize_pop <- function(nindiv=1000,maxage=80){
  # ages
    ages <- sample(0:(maxage-1),nindiv,replace = TRUE)
    ages <- sort(ages)

  # initial array
    init <- array(NA,dim=c(nindiv,maxage,6))
    colnames(init) <- c(paste0("Age",0:(maxage-1)))

  # # infection status matrix
  #   inf_stat_mat <- matrix(c(rep(NA, maxage*nindiv)),nrow=nindiv)
  #   colnames(inf_stat_mat) <- c(paste0("Age",0:(maxage-1)))
  # # years since last infection matrix
  #   x <- matrix(c(rep(999, maxage*nindiv)),nrow=nindiv)
  #   colnames(x) <- c(paste0("Age",0:(maxage-1)))
  # # vaccination history matrix
  #   vac_hist_mat <- matrix(c(rep(NA, maxage*nindiv)),nrow=nindiv)
  #   colnames(vac_hist_mat) <- c(paste0("Age",0:(maxage-1)))
  # # years since vaccination matrix
  #   v <- matrix(c(rep(999, maxage*nindiv)),nrow=nindiv)
  #   colnames(v) <- c(paste0("Age",0:(maxage-1)))
  # # susceptibility matrix
  #   suscept_mat <- matrix(c(rep(1, maxage*nindiv)),nrow=nindiv)
  #   colnames(suscept_mat) <- c(paste0("Age",0:(maxage-1)))
  # # infection status matrix
  #   lifetime_inf <- array(NA,dim=c(nindiv,maxage,years))
  #   colnames(lifetime_inf) <- c(paste0("Age",0:(maxage-1)))


  # create naive matrices for population for first year (0 = not infected, NA = not alive)
    for (i in 1:nindiv){
        init[i,,1] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # infection matrix
        init[i,,2] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # vaccination history matrix
        init[i,,3] <- c(rep(1,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # susceptibility matrix
        init[i,,4] <- c(rep(999,ages[i]+1),rep(NA,(maxage-1)-ages[i])) # years since infection matrix
        init[i,,5] <- c(rep(999,ages[i]+1),rep(NA,(maxage-1)-ages[i])) # years since vaccination matrix
        init[i,,6] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # lifetime infections matrix
    }

    return(init)
}
