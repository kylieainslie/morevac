### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

    
#' Initialize population
#'
#' This function initializes the population before running the model.
#' @param nindiv Number of individuals in the simulated population. Defaults to 1000.
#' @param years Number of years to simulate over. Defaults to 200.
#' @param maxage Maximum age of individuals after which they die and are removed from the population. Defaults to 80.
#' @return List containing initialized infection history matrix, vaccination history matrix, susceptibility matrix, attack rate vector, and incidence vector.
#' @keywords morevac
#' @export
#' @examples
#' initialize_pop()    
initialize_pop <- function(nindiv=1000,years=200,maxage=80){
  # ages
    ages <- sample(0:(maxage-1),nindiv,replace = TRUE)
    ages <- sort(ages)
  # infection status matrix
    inf_stat_mat <- array(NA,dim=c(nindiv,maxage,years))
    colnames(inf_stat_mat) <- c(paste0("Age",0:(maxage-1)))
  # years since last infection matrix  
    x <- matrix(c(rep(999, maxage*nindiv)),nrow=nindiv)
    colnames(x) <- c(paste0("Age",0:(maxage-1)))
  # vaccination history matrix
    vac_hist_mat <- array(NA,dim=c(nindiv,maxage,years))
    colnames(vac_hist_mat) <- c(paste0("Age",0:(maxage-1)))
  # susceptibility matrix
    suscept_mat <- array(1,dim=c(nindiv,maxage,years))
    colnames(suscept_mat) <- c(paste0("Age",0:(maxage-1)))
  # infection status matrix
    lifetime_inf <- array(NA,dim=c(nindiv,maxage,years))
    colnames(lifetime_inf) <- c(paste0("Age",0:(maxage-1)))
    
    
  # create naive matrices for population for first year (0 = not infected, NA = not alive)
    for (i in 1:nindiv){
        inf_stat_mat[i,,] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))
        vac_hist_mat[i,,] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))
         suscept_mat[i,,] <- c(rep(1,ages[i]+1),rep(NA,(maxage-1)-ages[i]))
                    x[i,] <- c(rep(999,ages[i]+1),rep(NA,(maxage-1)-ages[i]))
        lifetime_inf[i,,] <- c(rep(0,ages[i]+1),rep(NA,(maxage-1)-ages[i]))
    }  
    
  # attack rate  
    attack_rate <- c(rep(NA,years))
    attack_rate_by_age <- matrix(c(rep(NA,years*maxage)),nrow=years)
    colnames(attack_rate_by_age) <- c(paste0("Age",0:(maxage-1)))

    return(list(infection_matrix=inf_stat_mat,x=x,vac_history=vac_hist_mat,
                susceptibility=suscept_mat,attack_rate=attack_rate, 
                attack_rate_by_age=attack_rate_by_age,age=ages,lifetime_infections=lifetime_inf))
}
