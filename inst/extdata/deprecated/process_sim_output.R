### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Processes simulation output into a data frame
#'
#' This function initializes the population before running the model.
#' @param sim_out list output from simulations
#' @param year_range range of years to follow cohort
#' @param age_range range of ages to follow cohort
#' @return list of two data frames:
#'         1) data frame with attack rates averaged over simulations by year, age, and vaccination strategy
#'         2) data frame with lifetime infections by simulation, year, age, and vac strategy
#' @keywords morevac
#' @export
process_sim_output <- function(sim_out, year_range, age_range, ve_out = FALSE){
# attack rates
  rtn_ar <- data.frame(Year = c(rep(year_range,3)),
                       Attack_Rate = c(apply(sim_out[[1]]$attack_rate,1,mean),
                                       apply(sim_out[[2]]$attack_rate,1,mean),
                                       apply(sim_out[[3]]$attack_rate,1,mean)),
                       Lower = c(apply(sim_out[[1]]$attack_rate,1,FUN = function(x) quantile(x, c(0.025))),
                                 apply(sim_out[[2]]$attack_rate,1,FUN = function(x) quantile(x, c(0.025))),
                                 apply(sim_out[[3]]$attack_rate,1,FUN = function(x) quantile(x, c(0.025)))),
                       Upper = c(apply(sim_out[[1]]$attack_rate,1,FUN = function(x) quantile(x, c(0.975))),
                                 apply(sim_out[[2]]$attack_rate,1,FUN = function(x) quantile(x, c(0.975))),
                                 apply(sim_out[[3]]$attack_rate,1,FUN = function(x) quantile(x, c(0.975)))),
                       Age = as.factor(c(rep(age_range,3))),
                       Vac_Strategy = c(rep('No Vaccination',length(year_range)),
                                        rep('Annual',length(year_range)),
                                        rep('Biannual',length(year_range)))
                    )
# lifetime infections
  tmp1 <- as.data.frame(sim_out[[1]]$lifetime_infections)
  n_sim <- dim(tmp1)[2]
  tmp1 <- gather(tmp1, Sim, Lifetime_Infections, names(tmp1), factor_key=TRUE)
  tmp1$Sim <- as.factor(str_remove(tmp1$Sim, 'sim'))
  tmp1$Age <- as.factor(c(rep(age_range,n_sim)))
  tmp1$Vac_Strategy <- c(rep('No Vaccination',dim(tmp1)[1]))

  tmp2 <- as.data.frame(sim_out[[2]]$lifetime_infections)
  tmp2 <- gather(tmp2, Sim, Lifetime_Infections, names(tmp2), factor_key=TRUE)
  tmp2$Sim <- as.factor(str_remove(tmp2$Sim, 'sim'))
  tmp2$Age <- as.factor(c(rep(age_range,n_sim)))
  tmp2$Vac_Strategy <- c(rep('Annual',dim(tmp2)[1]))

  tmp3 <- as.data.frame(sim_out[[3]]$lifetime_infections)
  tmp3 <- gather(tmp3, Sim, Lifetime_Infections, names(tmp3), factor_key=TRUE)
  tmp3$Sim <- as.factor(str_remove(tmp3$Sim, 'sim'))
  tmp3$Age <- as.factor(c(rep(age_range,n_sim)))
  tmp3$Vac_Strategy <- c(rep('Biannual',dim(tmp3)[1]))

  rtn_li <- rbind(tmp1,tmp2,tmp3)

# ve
  if(ve_out == TRUE){
  rtn_ve <- data.frame(Year = c(rep(year_range,3)),
                       Attack_Rate = c(apply(sim_out[[1]]$ve,1,mean),
                                       apply(sim_out[[2]]$ve,1,mean),
                                       apply(sim_out[[3]]$ve,1,mean)),
                       Lower = c(apply(sim_out[[1]]$ve,1,FUN = function(x) quantile(x, c(0.025))),
                                 apply(sim_out[[2]]$ve,1,FUN = function(x) quantile(x, c(0.025))),
                                 apply(sim_out[[3]]$ve,1,FUN = function(x) quantile(x, c(0.025)))),
                       Upper = c(apply(sim_out[[1]]$ve,1,FUN = function(x) quantile(x, c(0.975))),
                                 apply(sim_out[[2]]$ve,1,FUN = function(x) quantile(x, c(0.975))),
                                 apply(sim_out[[3]]$ve,1,FUN = function(x) quantile(x, c(0.975)))),
                       Age = as.factor(c(rep(age_range,3))),
                       Vac_Strategy = c(rep('No Vaccination',length(year_range)),
                                        rep('Annual',length(year_range)),
                                        rep('Biannual',length(year_range)))
  )
  return(list(rtn_ar, rtn_li, rtn_ve))
  } else {return(list(rtn_ar, rtn_li))}
}
