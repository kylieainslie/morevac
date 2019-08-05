#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function initializes the population before running the model.
#' @param n number of individuals to be simulated
#' @param years vector of years to run simulation over (YYYY format)
#' @param max_age maximum age of an individual (removed from population after max_age)
#' @param vac_start_year year that vaccination starts (YYYY)
#' @param start_vac_age age at which an individual may be vaccinated
#' @param stop_vac_age age at which vaccination stops
#' @param vac_coverage vaccination coverage
#' @return list with two elements: 1) a list of infection histories and attack rates and
#'         2) a plot of annual attack rates by vaccination scenario
#' @keywords morevac
#' @export
get_attack_rates <- function(inf_history, ages_mat = NULL, vac_history = NULL){

  n <- nrow(inf_history)
  nyears <- ncol(inf_history)

  attack_rates <- colSums(inf_history)/nindiv

  # if (!is.null(vac_history)){
  #   ar_by_vac <-
  # }
  #
  if(!is.null(ages_mat)){
    rownames(inf_history) <- ages_mat[,1]
    # number of infections of each age
    age_sums <- rowsum(inf_history,ages_mat[,1])
    # number of individuals of each age
    counts <- count(as.data.frame(inf_history),rownames(inf_history))
    # divide number of infections by number in each age group
    ar_by_age <- age_sums/counts$n
    # reorder ar_by_age matrix to correspond to correct ages
    no_ages <- nrow(age_sums)
    shift <- 0
    for (j in 2:nyears){
      shift <- shift + 1
      if (shift == no_ages){shift <- 0}
      shift_row <- c(tail(ar_by_age[,j],shift), head(ar_by_age[,j],no_ages-shift))
      ar_by_age[,j] <- shift_row
     }
  }

  if (!is.null(ages_mat)){
    rtn <- list(attack_rates = attack_rates,
                ar_by_age = ar_by_age)
  } else {rtn <- attack_rates}

  return(rtn)
}
