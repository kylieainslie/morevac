### run simulations on cluster ###
# created: 23/01/2020
# last modified: 23/01/2020

run_sims_on_cluster <- function(n_sim, nindiv, years, betas, vac_cov, wane, take, epsilon, vac_protect, rho, file){
  ### run simulations
  cat("\n No vaccination simulation running... \n")
  # returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
  sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_vac, vac_strategy = 0,
                         wane = 1, take = 1, epsilon = 0, vac_protect = 0.7, rho = 0.9)
  cat("\n Annual vaccination simulation running... \n")
  sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Annual, vac_strategy = 1,
                         wane = 1, take = 1, epsilon = 0, vac_protect = 0.7, rho = 0.9)
  cat("\n Every other year vaccination simulation running... \n")
  sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Biennial, vac_strategy = 2,
                         wane = 1, take = 1, epsilon = 0, vac_protect = 0.7, rho = 0.9)

  # extract cohorts from each sim and combine raw inf and vac histories for every simulation
  sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = years, nsim = n_sim)
  sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = years, nsim = n_sim)
  sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = years, nsim = n_sim)

  # combine sim results into one data.table
  inf_histories <- rbindlist(list(No_Vac = sim0_results$inf_history, Annual = sim1_results$inf_history, Biennial = sim2_results$inf_history), idcol = 'Vac_Strategy')
  vac_histories <- rbindlist(list(No_Vac = sim0_results$vac_history, Annual = sim1_results$vac_history, Biennial = sim2_results$vac_history), idcol = 'Vac_Strategy')

  # write raw output to file
  try(data.table::fwrite(inf_histories, file = paste0(file,"_inf_hist.csv"), col.names = TRUE,
                         row.names = FALSE, sep = ","))
  try(data.table::fwrite(vac_histories, file = paste0(file,"_vac_hist.csv"), col.names = TRUE,
                         row.names = FALSE, sep = ","))
}
