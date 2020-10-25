### run simulations from parameter file ###
# created: 23/01/2020
# last modified: 25/10/2020

#' This function runs sims for each combination of parameter value from parameter input file creates
#' @param params_file character string indicating the name of the file with input parameter values
#' @param index sequence of rows to index params file by
#' @param out_file character string indicating name of output file
#' @param raw_out_file if true, raw infection and vaccination history files are output. If false, only summary files are output.
#' @return writes csv files to the working directory with infection and vaccination histories for every row of params_file
#' @keywords morevac
#' @importFrom data.table fread
#' @importFrom logitnorm rlogitnorm
#' @export
run_sims_all <- function(params_file,
                         index = NULL,
                         out_file = "test_",
                         raw_out_file = FALSE){

  ### read in parameter file
  params <- fread(params_file)

  if(!is.null(index)){
    params <- params[index,]
  }

  for (i in 1:nrow(params)){
  cat("\n Simulation ",i," of",nrow(params),"\n")

  ### parameter values
  n_sim = params$n_sim[i]
  n_indiv = params$n_indiv[i]
  max_age = params$max_age[i]
  years = params$start_year[i]:params$end_year[i]
  if (params$fixed_beta[i]){
    betas = c(params$pandemic_beta[i], rep(params$epidemic_beta[i],length(years)-1))
  } else {betas = c(params$pandemic_beta[i], rlogitnorm(length(years)-1, mu = -1.386, sigma = 0.35))}
  wane = params$wane[i]
  take = params$take[i]
  epsilon = params$exposure_penalty[i]
  vac_protect = params$vac_protect[i]
  rho = params$rho[i]
  vac_cutoff = params$vac_cutoff[i]

  vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
  vac_cov_dat$Annual[3:(vac_cutoff + 1)] <- params$vac_cov[i]
  vac_cov_dat$Biennial[seq(3,vac_cutoff+1,2)] <- params$vac_cov[i]

  ### run simulations
  # returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
  sim_results <- list()
  for(vac_strat in 1:3){
    sim_raw <- run_sim_2(sim = n_sim, n = n_indiv, years = years, betas = betas, vac_cov = vac_cov_dat[,vac_strat + 1],
                         vac_strategy = vac_strat - 1, wane = wane, take = take, epsilon = epsilon,
                         vac_protect = vac_protect, rho = rho)

    sim_results [[vac_strat]] <- sim_raw
    rm(sim_raw)
  }

  # combine sim results into one data.table
  inf_histories <- rbindlist(list(No_Vac = sim_results[[1]]$inf_history,
                                  Annual = sim_results[[2]]$inf_history,
                                  Biennial = sim_results[[3]]$inf_history),
                             idcol = 'Vac_Strategy') %>%
    mutate(Param_Index = params$id[i],
           Num_Infs = rowSums(select(.,Age0:Age18)))
  vac_histories <- rbindlist(list(No_Vac = sim_results[[1]]$vac_history,
                                  Annual = sim_results[[2]]$vac_history,
                                  Biennial = sim_results[[3]]$vac_history),
                             idcol = 'Vac_Strategy') %>%
    mutate(Param_Index = params$id[i],
           Num_Vacs = rowSums(select(.,Age0:Age18)))

  # determine mean infections and attack rates over simulations and bootstrap for CIs
  data_summary <- summarise_raw_output(id = params$id[i])

  ### Create summary output files
  # bind columns with parameter values
  mean_infs <- left_join(data_summary$mean_infs, params, by = c("Param_Index" = "id"))
  mean_diff <- left_join(data_summary$mean_diff, params, by = c("Param_Index" = "id"))
  mean_ar <- left_join(data_summary$mean_ar, params, by = c("Param_Index" = "id"))

  # write files to avoid having to read in raw data again
  data.table::fwrite(mean_infs, file = paste0("mean_infs_",out_file,params$id[i],".csv"),
                     col.names = TRUE, row.names = FALSE, sep = ",")
  data.table::fwrite(mean_diff, file = paste0("mean_diff_",out_file,params$id[i],".csv"), col.names = TRUE,
                     row.names = FALSE, sep = ",")
  data.table::fwrite(mean_ar, file = paste0("mean_ar_",out_file,params$id[i],".csv"), col.names = TRUE,
                     row.names = FALSE, sep = ",")


  # write raw output to file
    if(raw_out_file){
      try(data.table::fwrite(inf_histories, file = paste0(out_file, params$id[i],"_inf_hist.csv"), col.names = TRUE,
                             row.names = FALSE, sep = ","))
      try(data.table::fwrite(vac_histories, file = paste0(out_file, params$id[i],"_vac_hist.csv"), col.names = TRUE,
                             row.names = FALSE, sep = ","))
    }
  }
}
