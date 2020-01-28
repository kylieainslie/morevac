### run simulations on cluster ###
# created: 27/01/2020
# last modified: 27/01/2020

#' This function runs sims for each combination of parameter value from parameter input file creates
#' @param params either a dataframe with parameter sets or a character string indicating the name of the file with input parameter values
#' @param out_file character string indicating name of output file
#' @return writes csv files to the working directory with infection and vaccination histories for every row of params_file
#' @keywords morevac
#' @export
run_sims_clust <- function(n_sim = 20, n_indiv = 10000, max_age = 80,
                           start_year = 1820, end_year = 2028, pandemic_beta = 0.4,
                           epidemic_beta = 0.2,wane = 1, take = 1, epsilon = 0,
                           vac_protect = 0.7, rho = 0.9, vac_cutoff = 10, out_file = "test"){

  ### parameter values
  years = start_year:end_year
  betas = c(pandemic_beta, rep(epidemic_beta,length(years)-1))

  vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
  vac_cov_dat$Annual[3:(vac_cutoff + 1)] <- vac_cov
  vac_cov_dat$Biennial[seq(3,vac_cutoff+1,2)] <- vac_cov
  ### run simulations
  # cat("\n No vaccination simulation running... \n")
  # returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
  sim_test0 <- run_sim_2(sim = n_sim, n = n_indiv, years = years, betas = betas, vac_cov = vac_cov_dat$No_vac, vac_strategy = 0,
                         wane = wane, take = take, epsilon = epsilon, vac_protect = vac_protect, rho = rho)
  # cat("\n Annual vaccination simulation running... \n")
  sim_test1 <- run_sim_2(sim = n_sim, n = n_indiv, years = years, betas = betas, vac_cov = vac_cov_dat$Annual, vac_strategy = 1,
                         wane = wane, take = take, epsilon = epsilon, vac_protect = vac_protect, rho = rho)
  # cat("\n Every other year vaccination simulation running... \n")
  sim_test2 <- run_sim_2(sim = n_sim, n = n_indiv, years = years, betas = betas, vac_cov = vac_cov_dat$Biennial, vac_strategy = 2,
                         wane = wane, take = take, epsilon = epsilon, vac_protect = vac_protect, rho = rho)

  # extract cohorts from each sim and combine raw inf and vac histories for every simulation
  sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = years, nsim = n_sim)
  sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = years, nsim = n_sim)
  sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = years, nsim = n_sim)

  # combine sim results into one data.table
  inf_histories <- rbindlist(list(No_Vac = sim0_results$inf_history, Annual = sim1_results$inf_history, Biennial = sim2_results$inf_history), idcol = 'Vac_Strategy')
  vac_histories <- rbindlist(list(No_Vac = sim0_results$vac_history, Annual = sim1_results$vac_history, Biennial = sim2_results$vac_history), idcol = 'Vac_Strategy')

  # write raw output to file
  try(data.table::fwrite(inf_histories, file = paste0(out_file,params$ID[1],"_inf_hist.csv"), col.names = TRUE,
                         row.names = FALSE, sep = ","))
  try(data.table::fwrite(vac_histories, file = paste0(out_file,params$ID[1],"_vac_hist.csv"), col.names = TRUE,
                         row.names = FALSE, sep = ","))
  #}
  return(NULL)
}
