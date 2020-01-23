### create parameter file

create_params_file <- function(n_sim = 100, n_indiv = 30000, max_age = 80, vac_cutoff = 10,
                               lhc_size = 1000, out_file = "parameter_values", seed = NULL){

  if(!is.null(seed)){set.seed(seed)}

  # generate latin hypercube parameters
  mylhc <- randomLHS(lhc_size, 6)
  colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
  # output
  rtn <- tibble(n_sim = c(rep(n_sim, lhc_size)),
                n_indiv = c(rep(n_indiv, lhc_size)),
                max_age = c(rep(max_age, lhc_size)),
                vac_cutoff = c(rep(vac_cutoff, lhc_size)),
                vac_cov = qunif(mylhc[,"Vac_Cov"], min = 0, max = 0.5),
                exposure_penalty = qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.1),
                wane = mylhc[,"Waning"],
                take = qunif(mylhc[,"Take"], min = 0.5, max = 1),
                rho = mylhc[,"Rho"],
                vac_protect = mylhc[,"VE"]
                )
  data.table::fwrite(rtn, file = paste0(out_file,".csv"), col.names = TRUE,
                     row.names = FALSE, sep = ",")
  return(rtn)
}
