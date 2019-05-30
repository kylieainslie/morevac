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
                    flag){
### create empty arrays for storing information about each simulation
  out <- array(NA,dim=c(200,80,sim))
  life_inf <- matrix(c(rep(NA,sim*length(age_range))),nrow=sim)
  ar_out <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))

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
    test <- multiannual2(n = nindiv, vac_coverage = vaccov, suscept_func_version = version, biannual = vac_strat,  rho = rho)
    # attack rate by age
    out[,,s] <- test$attack_rate_by_age
    dimnames(out)[[1]] <- rownames(test$attack_rate_by_age)
    ar_out[,s] <- diag(out[rownames(out) %in% year_range,(age_range+1),s])

    # lifetime infections

  }
  close(pb)

  return(ar_out)
}
### output
  # cohort <- data.frame(Year = c(rep(year_range,3)),
  #                      Attack_Rate = c(apply(ar_out0,1,mean),
  #                                      apply(ar_outa,1,mean),
  #                                      apply(ar_outb,1,mean)),
  #                      Lower = c(apply(ar_out0,1,FUN = function(x) quantile(x, c(0.025))),
  #                                apply(ar_outa,1,FUN = function(x) quantile(x, c(0.025))),
  #                                apply(ar_outb,1,FUN = function(x) quantile(x, c(0.025)))),
  #                      Upper = c(apply(ar_out0,1,FUN = function(x) quantile(x, c(0.975))),
  #                                apply(ar_outa,1,FUN = function(x) quantile(x, c(0.975))),
  #                                apply(ar_outb,1,FUN = function(x) quantile(x, c(0.975)))),
  #                      Age = c(rep(age_range,3)),
  #                      Vac_Strategy = c(rep('No Vaccination',length(year_range)),
  #                                       rep('Annual',length(year_range)),
  #                                       rep('Biannual',length(year_range)))
  # )
  #
  # p_cohort <- plot_attack_rates(dat = cohort, by_vac = TRUE, c_bands = TRUE)
  #
  # pdf(file = paste0(filename,"_ar.pdf"))
  # plot(p_cohort)
  # dev.off()
  # #
  # # lifetime infections
  # life_inf_dat <- data.frame(Sim = c(rep(1:sim,3)),
  #                            Vac_Strategy = c(rep('No Vaccination',sim),
  #                                             rep('Annual',sim),
  #                                             rep('Biannual',sim)),
  #                            rbind(life_inf0,life_infa,life_infb)
  # )
  # names(life_inf_dat) <- c('Sim','Vac_Strategy',c(paste0("Age",age_range)))
  # data_long <- gather(life_inf_dat, Age, Life_Inf, Age0:Age19, factor_key=TRUE)
  # data_long$Age <- as.factor(str_remove(data_long$Age, 'Age'))
  #
  # data_long$Age = with(data_long, reorder(Age, Life_Inf, mean))
  #
  # p1 <- plot_lifetime_infections(dat = data_long, by_vac = TRUE)
  #
  # pdf(file = paste0(filename,"_life_inf.pdf"))
  # plot(p1)
  # dev.off()




