#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function post processes simulation results for a rolling cohort.
#' @param sim0 array of simulations results (no vac)
#' @param sim1 array of simulations results (annual vac)
#' @param sim2 array of simulation results (biannual vac)
#' @param nsim number of simulations performed (must be the same for each sim case)
#' @param total_year_range vector of years simulations were run over (YYYY format)
#' @param cohort_year_index vector of index of years to get attack rates for
#' @return data frame of attack rates within the cohort by year
#' @keywords morevac
#' @export
postprocess_sim_results_for_rolling_cohort <- function(simdat, nsim = 100, total_year_range = 1820:2028, length_study = 19){
  avg_ar <- matrix(numeric(nsim*length_study), nrow = length_study)
  rownames(avg_ar) <- paste0("Age",0:(length_study-1))
  colnames(avg_ar) <- paste0("Sim",1:nsim)

  # subset birth cohort
  for (s in 1:nsim){
    my_cohorts <- get_cohorts(inf_history = simdat$inf_history[,,s],
                              vac_history = simdat$vac_history[,,s],
                              ages = simdat$ages[,,s],
                              total_year_range = total_year_range)
    # calculate avg attack rate over cohorts
    cohort_sizes <- sapply(my_cohorts$cohort_ids, length)
    ninfs <- sapply(my_cohorts$inf_hist,function(x) apply(x,2,sum))
    colnames(ninfs) <- paste0("Cohort",1:10)
    cohort_ar <- sweep(ninfs, 2, cohort_sizes, FUN="/")
    avg_ar[,s] <- apply(cohort_ar,1,median)

    # calculate avg num lifetime infections by number of vaccinations
    lifetime_infs <- sapply(my_cohorts$inf_hist,function(x) apply(x,1,sum))
    num_vacs <- sapply(my_cohorts$vac_hist,function(x) apply(x,1,sum))
    tmp <- data.frame(Lifetime_Infs = unlist(lifetime_infs), Num_Vacs = unlist(num_vacs))
    avg_tmp <- ddply(tmp,~Num_Vacs,summarise,med=median(Lifetime_Infs))
    avg_tmp$sim <- c(rep(s,dim(avg_tmp)[1]))

    if (s == 1){avg_lifetime_infs <- avg_tmp
    } else {avg_lifetime_infs <- rbind(avg_lifetime_infs, avg_tmp)}

  }
  rtn <- list(Attack_Rate = t(avg_ar),
              Lifetime_Infections = avg_lifetime_infs)
  return(rtn)
}
