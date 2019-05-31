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
                    year_range,
                    age_range,
                    vaccov = 0.5,
                    version = 1,
                    biannual = FALSE,
                    rho = 0,
<<<<<<< Updated upstream
                    flag = 'annual'){
=======
                    flag = 'no vaccination'){
>>>>>>> Stashed changes
### create empty arrays for storing information about each simulation
  out <- array(NA,dim=c(200,80,sim))
  life_inf <- matrix(c(rep(NA,sim*length(age_range))),nrow=sim)
  ar_out <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
  rownames(ar_out) <- year_range
  colnames(ar_out) <- paste0(c(rep("sim",sim)),1:sim)
  lti_out <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
  rownames(lti_out) <- age_range
  colnames(lti_out) <- paste0(c(rep("sim",sim)),1:sim)

  # determine scenario by flag
    if (flag == 'no vaccination'){
      vaccov <- 0
      vac_strat <- FALSE
    } else if (flag == 'annual'){
      vac_strat <- FALSE
    } else if (flag == 'biannual'){
      vac_strat <- TRUE
    }
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
                         biannual = vac_strat,  rho = rho
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

  return(list(attack_rate = ar_out,
              lifetime_infections = lti_out)
        )
}





