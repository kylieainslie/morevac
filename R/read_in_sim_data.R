### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Read in simulation output
#'
#' This function initializes the population before running the model.
#' @param tags list filename tags (should be of length 3)
#' @return list of attack rate and lifetime infections data for each vac scenario
#' @keywords morevac
#' @export
read_in_sim_data <- function(tags){

  rtn <- list(no_vac = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags[1],".csv"),header = TRUE)[,-1],
                            lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags[1],".csv"),header = TRUE)[,-1]),
              annual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags[2],".csv"),header = TRUE)[,-1],
                            lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags[2],".csv"),header = TRUE)[,-1]),
              biannual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags[3],".csv"),header = TRUE)[,-1],
                              lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags[3],".csv"),header = TRUE)[,-1])
  )

  return(rtn)

}
