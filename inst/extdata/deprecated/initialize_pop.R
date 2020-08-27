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
    ages <- sort(sample(0:(maxage-1),nindiv,replace = TRUE))

  # initial array
    init <- array(NA,dim=c(nindiv,maxage,5))
    colnames(init) <- c(paste0("Age",0:(maxage-1)))
    rownames(init) <- ages

  # create naive matrices for population for first year (0 = not infected, NA = not alive)
    for (i in 1:nindiv){
        init[i,,1] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # infection matrix
        init[i,,2] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # vaccination history matrix
        init[i,,3] <- c(rep(1,ages[i]+1),rep(NA,(maxage-1)-ages[i]))   # susceptibility matrix
        init[i,,4] <- c(rep(999,ages[i]+1),rep(NA,(maxage-1)-ages[i])) # years since infection matrix
        init[i,,5] <- c(rep(999,ages[i]+1),rep(NA,(maxage-1)-ages[i])) # years since vaccination matrix
    }

    return(init)
}
