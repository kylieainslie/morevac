#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function initializes the population before running the model.
#' @param inf_history matrix of infection histories for each person
#' @param ages_mat matrix of ages of each person for every year
#' @param vac_history matrix of vaccination history for each person
#' @param years vector of years to run simulation over (YYYY format)
#' @return data frame or list of data frames with attack rates
#' @keywords morevac
#' @export
get_attack_rates <- function(inf_history, ages_mat = NULL, vac_history = NULL, years){

  n <- nrow(inf_history)
  nyears <- ncol(inf_history)

  attack_rates <- colSums(inf_history)/n
  attack_rate_dat <- data.frame(Year = years, Attack_Rate = attack_rates)
  if (!is.null(vac_history)){
    ar_vac <- numeric(nyears)
    ar_unvac <- numeric(nyears)
    # number of individuals vaccinated in each year
    for (j in 1:nyears){
      vac_indivs <- which(vac_history[,j]==1)
      ar_vac[j] <- ifelse(length(vac_indivs) == 0, 0, sum(inf_history[vac_indivs,j])/length(vac_indivs))
      ar_unvac[j] <- ifelse(length(vac_indivs) == 0, sum(inf_history[,j])/n, sum(inf_history[-vac_indivs,j])/(n - length(vac_indivs)))
    }
    ar_vac <- ifelse(is.nan(ar_vac),0,ar_vac)
    ar_by_vac_dat <- data.frame(Year = rep(years,2), Vac_Status = c(rep("Vaccinated",nyears),rep("Unvaccinated",nyears)),
                                Attack_Rate = c(ar_vac,ar_unvac))
    ar_by_vac_dat <- ar_by_vac_dat[order(ar_by_vac_dat$Year),]
  }

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

  if (!is.null(ages_mat) & is.null(vac_history)){
    rtn <- list(attack_rates = attack_rate_dat,
                ar_by_age = ar_by_age_dat)
  } else if (is.null(ages_mat) & !is.null(vac_history)){
    rtn <- list(attack_rates = attack_rate_dat,
                ar_by_vac = ar_by_vac_dat)
  } else if (!is.null(ages_mat) & !is.null(vac_history)){
    rtn <- list(attack_rates = attack_rate_dat,
                ar_by_vac = ar_by_vac_dat,
                ar_by_age = ar_by_age_dat)
  } else {rtn <- attack_rate_dat}

  return(rtn)
}
