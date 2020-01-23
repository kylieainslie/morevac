### create parameter file

#' @param n_sim number of simulations
#' @param n_indiv number of individuals
#' @param max_age maximum age of individuals
#' @param vac_cutoff age at which vaccination stops
#' @param start_year start simulation year (YYYY format)
#' @param end_year end simulation year (YYYY format)
#' @param pandemic_beta value of pandemic force of infection
#' @param epidemic_beta value of epidemic force of infection
#' @param lhc_size number of latin hypercube parameter combinations
#' @param out_file name of output file
#' @return tibble with parameter values and writes csv file to the working directory with parameter values
#' @keywords morevac
#' @export
create_params_file <- function(n_sim = 100, n_indiv = 30000, max_age = 80, vac_cutoff = 10,
                               start_year = 1820, end_year = 2028, pandemic_beta = 0.4, epidemic_beta = 0.2,
                               lhc_size = 1000, out_file = "parameter_values", seed = NULL){

  if(!is.null(seed)){set.seed(seed)}

  # generate latin hypercube parameters
  mylhc <- randomLHS(lhc_size, 6)
  colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
  # output
  rtn <- tibble(n_sim = c(rep(n_sim, lhc_size)),
                n_indiv = c(rep(n_indiv, lhc_size)),
                max_age = c(rep(max_age, lhc_size)),
                start_year = c(rep(start_year, lhc_size)),
                end_year = c(rep(end_year, lhc_size)),
                pandemic_beta = c(rep(pandemic_beta, lhc_size)),
                epidemic_beta = c(rep(epidemic_beta, lhc_size)),
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
