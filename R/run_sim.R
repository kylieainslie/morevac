### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Run simulations of multi-annual model of infection and vaccination
#'
#' This function initializes the population before running the model.
#' @param sim number of simulations to run
#' @param nindiv number of individuals to be simulated
#' @param year_range range of years to follow cohort
#' @param age_range range of ages to follow cohort
#' @param vaccov vaccination coverage, should be between 0 and 1 (inclusive)
#' @param filename prefix of outputted figures
#' @param version which vaccination version to use: 1 = either-or, 2 = multiplicative
#' @return two plots 1) plot of annual attack rates for the cohort and 2) a plot of number of lifetime infections by age
#' @keywords morevac
#' @export

run_sim <- function(sim = 100,
                    nindiv = 1000,
                    year_range = c(2000:2019),
                    age_range = c(0:19),
                    vaccov = 0.5,
                    version = 1,
                    vac_strategy = 1,
                    rho = 0,
                    file.out = FALSE){

### create empty arrays for storing information about each simulation
  out <- array(NA,dim=c(200,80,sim))
  life_inf <- matrix(c(rep(NA,sim*length(age_range))),nrow=sim)
  ar_out <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
  rownames(ar_out) <- year_range
  colnames(ar_out) <- paste0(c(rep("sim",sim)),1:sim)
  lti_out <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
  rownames(lti_out) <- age_range
  colnames(lti_out) <- paste0(c(rep("sim",sim)),1:sim)

  if (vac_strategy == 0){vaccov <- 0}
### create progress bar
  pb <- txtProgressBar(min = 0, max = sim, style = 3)
### start simulations
  for (s in 1:sim){
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, s)

    # run model
    test <- multiannual2(n = nindiv, vac_coverage = vaccov,
                         suscept_func_version = version,
                         vac_strategy = vac_strategy,  rho = rho
                         )
    # attack rate by age
    out[,,s] <- test$attack_rate_by_age
    dimnames(out)[[1]] <- rownames(test$attack_rate_by_age)
    ar_out[,s] <- diag(out[rownames(out) %in% year_range,(age_range+1),s])
    # lifetime infections
    lifetime_inf <- get_lifetime_inf(test$history[,,1])
    lti_out[,s] <- lifetime_inf[length(age_range),1:length(age_range)]
  }
  close(pb)
  if (file.out == TRUE){
    cat("Writing output to file...","\n")
    write.csv(ar_out,
              file = '/Volumes/kainslie/morevac_sims/data/attack_rate_data.csv')
    write.csv(lti_out,
              file = '/Volumes/kainslie/morevac_sims/data/lifetime_inf_data.csv')
  }
  return(list(attack_rate = ar_out,
              lifetime_infections = lti_out)
        )
}





