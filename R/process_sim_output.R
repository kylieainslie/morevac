### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Processes simulation output into a data frame
#'
#' This function initializes the population before running the model.
#' @param sim_out list output from simulations
#' @param j index of first list level
#' @param year_range range of years to follow cohort
#' @param age_range range of ages to follow cohort
#' @return data frame with attack rate and lifetime infections by year, age, and vaccination strategy
#' @keywords morevac
#' @export
process_sim_output <- function(sim_out, j, year_range, age_range){

  rtn <- data.frame(Year = c(rep(year_range,3)),
                    Attack_Rate = c(apply(sim_out[[j]][[1]]$attack_rate,1,mean),
                                    apply(sim_out[[j]][[2]]$attack_rate,1,mean),
                                    apply(sim_out[[j]][[3]]$attack_rate,1,mean)),
                    Lower = c(apply(sim_out[[j]][[1]]$attack_rate,1,FUN = function(x) quantile(x, c(0.025))),
                              apply(sim_out[[j]][[2]]$attack_rate,1,FUN = function(x) quantile(x, c(0.025))),
                              apply(sim_out[[j]][[3]]$attack_rate,1,FUN = function(x) quantile(x, c(0.025)))),
                    Upper = c(apply(sim_out[[j]][[1]]$attack_rate,1,FUN = function(x) quantile(x, c(0.975))),
                              apply(sim_out[[j]][[2]]$attack_rate,1,FUN = function(x) quantile(x, c(0.975))),
                              apply(sim_out[[j]][[3]]$attack_rate,1,FUN = function(x) quantile(x, c(0.975)))),
                    Lifetime_Infections = c(apply(sim_out[[j]][[1]]$lifetime_infections,1,mean),
                                            apply(sim_out[[j]][[2]]$lifetime_infections,1,mean),
                                            apply(sim_out[[j]][[3]]$lifetime_infections,1,mean)),
                    Lower2 = c(apply(sim_out[[j]][[1]]$lifetime_infections,1,FUN = function(x) quantile(x, c(0.025))),
                               apply(sim_out[[j]][[2]]$lifetime_infections,1,FUN = function(x) quantile(x, c(0.025))),
                               apply(sim_out[[j]][[3]]$lifetime_infections,1,FUN = function(x) quantile(x, c(0.025)))),
                    Upper2 = c(apply(sim_out[[j]][[1]]$lifetime_infections,1,FUN = function(x) quantile(x, c(0.975))),
                               apply(sim_out[[j]][[2]]$lifetime_infections,1,FUN = function(x) quantile(x, c(0.975))),
                               apply(sim_out[[j]][[3]]$lifetime_infections,1,FUN = function(x) quantile(x, c(0.975)))),
                    Age = c(rep(age_range,3)),
                    Vac_Strategy = c(rep('No Vaccination',length(year_range)),
                                     rep('Annual',length(year_range)),
                                     rep('Biannual',length(year_range)))
                    )
  return(rtn)
}
