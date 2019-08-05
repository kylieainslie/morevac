#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function initializes the population before running the model.
#' @param inf_history
#' @param ages_mat
#' @param vac_history
#' @param years vector of years to run simulation over (YYYY format)
#' @return data frame or list of data frames with attack rates
#' @keywords morevac
#' @export
get_attack_rates <- function(inf_history, ages_mat = NULL, vac_history = NULL, years){

  n <- nrow(inf_history)
  nyears <- ncol(inf_history)

  attack_rates <- colSums(inf_history)/n
  attack_rate_dat <- data.frame(Year = years, Attack_Rate = attack_rates)
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
    # convert to long data.frame
    ar_by_age_dat <- data.frame(Age = rownames(ar_by_age),ar_by_age)
    colnames(ar_by_age_dat) <- c("Age", paste0("Year",years))
    ar_by_age_dat <- gather(ar_by_age_dat, Year, Attack_Rate, paste0("Year",years), factor_key=TRUE)
    ar_by_age_dat$Year <- as.factor(str_remove(ar_by_age_dat$Year, 'Year'))
  }

  if (!is.null(ages_mat)){
    rtn <- list(attack_rates = attack_rate_dat,
                ar_by_age = ar_by_age_dat)
  } else {rtn <- attack_rate_dat}

  return(rtn)
}
