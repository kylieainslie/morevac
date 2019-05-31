### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function initializes the population before running the model.
#' @param n number of individuals to be simulated
#' @param years number of years to run simulation over
#' @param maxage maximum age of an individual (removed from population after maxage)
#' @param start_year year to start simulation
#' @param vac_start_year year that vaccination starts
#' @param start_vac_age age at which an individual may be vaccinated
#' @param vac_coverage vaccination coverage
#' @param beta_pandemic force of infection when simulation starts
#' @param beta_epidemic annual force of infection (after initialization of model)
#' @param delta_x drift parameter from natural infection
#' @param delta_v drift parameter from vaccination
#' @param mygamma protective effect of vaccine
#' @param suscept_func_version integer indicating which susceptibility version to use.
#'        1 = either-or, 2 = multiplicative.
#' @param biannual logical. annual or biannual vaccination?
#' @param rho correlation of vaccination
#' @param return_ar_only 0 = no, 1 = return annual attack rates, 2 = return attack rate by age
#' @return list with two elements: 1) a list of infection histories and attack rates and
#'         2) a plot of annual attack rates by vaccination scenario
#' @keywords morevac
#' @export
multiannual2 <- function(n = 1000,
                         years = 200,
                         maxage = 80,
                         start_year = 1820,
                         start_vac_year = 2000,
                         start_vac_age = 3,
                         vac_coverage = 0.5,
                         beta_pandemic = 0.4,
                         beta_epidemic = 0.15,
                         delta_x = 0.2,
                         delta_v = 0.2,
                         mygamma = 0.4,
                         suscept_func_version = 1,
                         biannual = FALSE,
                         rho = 0,
                         return_ar_only = 0
                         ){

  init <- initialize_pop(nindiv = n, maxage = maxage)
  # rename matrix from init array
  inf_hist_mat <- init[,,1]
  vac_hist_mat <- init[,,2]
   suscept_mat <- init[,,3]
             x <- init[,,4]
             v <- init[,,5]
  #lifetime_inf <- init[,,6]

  ages <- as.numeric(rownames(init))

  end_year <- start_year + years - 1

  attack_rate <- c(rep(NA,years))
  attack_rate_by_age <- matrix(c(rep(NA, years*maxage)),nrow = years)
  colnames(attack_rate_by_age) <- c(paste0("Age",0:(maxage-1)))
  rownames(attack_rate_by_age) <- start_year:end_year

  # year counter
    year_counter <- 1
    actual_year <- start_year
  # start loop over years
  while (year_counter < years+1){
    #print(year_counter)
    #print(lifetime_inf)
    inf_counter <- matrix(c(rep(0,maxage*2)),nrow=2)
    rownames(inf_counter) <- c('number_of_infections','n_age')
    colnames(inf_counter) <- c(paste0("Age",0:(maxage-1)))

    # turn off vaccination until start_vac_year
    if (actual_year<start_vac_year){vc <- 0
    } else {vc <- vac_coverage}

    # generate random numbers for infection and vaccination
    rn_inf <- runif(n,0,1)
    rn_vac <- runif(n,0,1)
    # loop over individuals
      i <- 1
    while(i < n+1){
      #print(i)
      a <- ages[i] + 1
      inf_counter[2,a] <- inf_counter[2,a] + 1

      if(is.na(x[i,a])){x[i,a]<-999}
      if(is.na(v[i,a])){v[i,a]<-999}
      if(is.na(inf_hist_mat[i,a])){inf_hist_mat[i,a]<-0}
      if(is.na(vac_hist_mat[i,a])){vac_hist_mat[i,a]<-0}

      # determine who will be vaccinated
        if (actual_year >= start_vac_year & ages[i] >= start_vac_age){
        #  vac_hist_mat[i,a] <- vaccinate(year = year_counter, vc = vc, vac_strategy = biannual)
        # } else {vac_hist_mat[i,a] <- 0} # set vaccination status to 0 for current year

        # v[i,a] <- v[i,a]*(1-vac_hist_mat[i,a])
        # incorporate prior vaccination
          if (biannual & (year_counter %% 2) != 0){
            if (a > 2 & vac_hist_mat[i,a-2] == 1){rn_vac[i] <- rn_vac[i] * (1-rho)}
          } else {
            if (a > 1 & vac_hist_mat[i,a-1] == 1){rn_vac[i] <- rn_vac[i] * (1-rho)}
          }

         # vaccinate
           if (rn_vac[i] <= vc & actual_year >= start_vac_year){
             if (biannual == FALSE) {
                vac_hist_mat[i,a] <- 1
                v[i,a] <- 0
              } else if (biannual == TRUE & (year_counter %% 2) != 0){
                      vac_hist_mat[i,a] <- 1
                      v[i,a] <- 0
              } else {vac_hist_mat[i,a] <- 0}
            } else {vac_hist_mat[i,a] <- 0}
         } else {vac_hist_mat[i,a] <- 0}
        #print(vac_hist_mat[i,])

      # calculate susceptibility function for person i
        suscept_mat[i,a] <- suscept_func_cpp(inf_history = x[i,a],
                                             vac_history = v[i,a],
                                             gamma = mygamma,
                                             drift_x = delta_x,
                                             drift_v = delta_v,
                                             version = suscept_func_version)

        # for first year have pandemic transmission rate
        if (year_counter==1){
          mybeta <-beta_pandemic
        } else {mybeta <- beta_epidemic}

      # infect person i if random number < beta*susceptability
        inf_hist_mat[i,a] <- infect_cpp(susceptibility = suscept_mat[i,a], foi = mybeta, randnum_inf = rn_inf[i])
        x[i,a] <- x[i,a]*(1-inf_hist_mat[i,a])
        inf_counter[1,a] <- inf_counter[1,a] + inf_hist_mat[i,a]

        if (a<maxage){
          x[i,a+1] <- x[i,a]+1
          v[i,a+1] <- v[i,a]+1
        }
      # update ages
        if (year_counter < years){
          ages[i] <- ages[i] + 1
          if (ages[i]==maxage){
            ages[i] <- 0
          }
        }
      # next person
          i <- i + 1
    } # end loop over individuals

  # calculate attack rate by age
    attack_rate[year_counter] <- sum(inf_counter[1,])/sum(inf_counter[2,])
    attack_rate_by_age[year_counter,] <- inf_counter[1,]/inf_counter[2,]

  # reset history for new naive individuals
    if (year_counter < years){
      age0 <- which(ages==0)
      #print(age0)
      inf_hist_mat[age0,] <- c(0,rep(NA,maxage-1))
      vac_hist_mat[age0,] <- c(0,rep(NA,maxage-1))
       suscept_mat[age0,] <- c(1,rep(NA,maxage-1))
                 x[age0,] <- c(999,rep(NA,maxage-1))
                 v[age0,] <- c(999,rep(NA,maxage-1))
    }
    #lifetime_inf[age0,] <- c(0,rep(NA,maxage-1))
    #print(vac_hist_mat)
    age0 <- which(ages==0)

    inf_hist_mat[age0,] <- c(0,rep(NA,maxage-1))
    vac_hist_mat[age0,] <- c(0,rep(NA,maxage-1))
     suscept_mat[age0,] <- c(1,rep(NA,maxage-1))
               x[age0,] <- c(999,rep(NA,maxage-1))
               v[age0,] <- c(999,rep(NA,maxage-1))

  # update counters
    year_counter <- year_counter + 1
    actual_year <- actual_year + 1
 } # end loop over years

### output
  # overwrite init with final matrices
  init[,,1] <- inf_hist_mat
  init[,,2] <- vac_hist_mat
  init[,,3] <- suscept_mat
  init[,,4] <- x
  init[,,5] <- v
  rownames(init) <- ages

  dat <- data.frame(Year=start_year:end_year,Attack_Rate=attack_rate)

  if (return_ar_only == 1){
    rtn <- dat
  } else if (return_ar_only == 2){
    rtn <- attack_rate_by_age
  } else {
    rtn <- list(history=init,
                attack_rate=dat,
                attack_rate_by_age = attack_rate_by_age
                )
  }

  return(rtn)
}
