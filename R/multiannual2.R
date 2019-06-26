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
#' @param vac_strategy Integer value indicating frequency of vaccination (1 = annual, 2 = biannual, 3 =triannual,...)
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
                         delta_x = NULL,
                         delta_v = NULL,
                         vac_protect = 0.7,
                         suscept_func_version = 1,
                         vac_strategy = 1,
                         rho = 0,
                         return_ar_only = 0
                         ){
# initialize the population
  init <- initialize_pop(nindiv = n, maxage = maxage)
  # rename matrix from init array
  inf_hist_mat <- init[,,1]
  vac_hist_mat <- init[,,2]
   suscept_mat <- init[,,3]
             x <- init[,,4]
             v <- init[,,5]
  ages <- as.numeric(rownames(init))

  end_year <- start_year + years - 1
# create empty vectors
  # vaccine strain update
    vaccine_dist <- c(rep(NA,years))
    update <- c(rep(NA,years))
    years_since_vac_update <- c(rep(NA,years))
  # attack rate and ve
    attack_rate <- c(rep(NA,years))
    ve <- attack_rate
    attack_rate_by_age <- matrix(c(rep(NA, years*maxage)),nrow = years)
    colnames(attack_rate_by_age) <- c(paste0("Age",0:(maxage-1)))
    rownames(attack_rate_by_age) <- start_year:end_year
  # infection counter
    null_inf_counter <- matrix(c(rep(0,maxage*6)),nrow=6)
    rownames(null_inf_counter) <- c('total_num_infections',
                             'n_age',
                             'num_vac_infections',
                             'num_vaccinated',
                             'num_unvac_infections',
                             'num_unvaccinated'
    )
    colnames(null_inf_counter) <- c(paste0("Age",0:(maxage-1)))

  # calculate drift for each year
    drift <- drift_func(years = years)
  # year counter
    year_counter <- 1
    actual_year <- start_year
    if (actual_year %% 2 == 0){
      ey <-1
    } else {ey <- 0}

# start loop over years
  while (year_counter < years+1){
  # initialize infection counter for current year
    inf_counter <- null_inf_counter
  # set drift value to year_counter value in drift matrix
    if(is.null(delta_x)){delta_x <- drift[year_counter]}
  # turn off vaccination until start_vac_year
    if (vac_strategy == 0 | actual_year<start_vac_year) {
      vc <- 0
      mygamma <- 1 - vac_protect
    } else {
      if (actual_year == start_vac_year){years_since_vac_update[year_counter] <- 0}
        vc <- vac_coverage
      # determine vaccine distance from circulating strain
        vaccine_dist[year_counter] <- min(1,sum(drift[(year_counter-years_since_vac_update[year_counter]):year_counter]))
      # update vaccine?
        update[year_counter] <- vaccine_update(years_since_vac_update = years_since_vac_update[year_counter],
                                               accumulated_drift = vaccine_dist[year_counter])
      # change years since vac update to 0 if updated in current year
        years_since_vac_update[year_counter] <- years_since_vac_update[year_counter] * (1 - update[year_counter]) + (1 - update[year_counter])

      # determine protective effect of vaccine based on distance from circulating strain
        mygamma <- (1-vac_protect)*(1-(vaccine_dist[year_counter]*(1-update[year_counter])))
    }
  # generate random numbers for infection and vaccination
    rn_inf <- runif(n,0,1)
    rn_vac <- runif(n,0,1)
  # loop over individuals
      i <- 1
    while(i < n+1){
      #print(i)
      a <- ages[i] + 1
      inf_counter[2,a] <- inf_counter[2,a] + 1
    # update current year value from NA to 999 or 0
      if(is.na(x[i,a])){x[i,a]<-999}
      if(is.na(v[i,a])){v[i,a]<-999}
      if(is.na(inf_hist_mat[i,a])){inf_hist_mat[i,a]<-0}
      if(is.na(vac_hist_mat[i,a])){vac_hist_mat[i,a]<-0}
    # determine whether an individual was vaccinated in the year (or two) previously
      if (a-vac_strategy < 1){prior_vac <- 0
      } else {prior_vac <- vac_hist_mat[i,a-vac_strategy]}

      # determine who will be vaccinated
       vac_hist_mat[i,a] <- vaccinate_cpp(prior_vac = prior_vac,
                                          even_year = ey,
                                          vac_cov = vc,
                                          vac_strategy = vac_strategy,
                                          age = ages[i],
                                          rho = rho,
                                          randnum_vac = rn_vac[i],
                                          actual_year = actual_year,
                                          start_vac_year = start_vac_year,
                                          start_vac_age = start_vac_age)
        v[i,a] <- v[i,a]*(1-vac_hist_mat[i,a])
      # update number of vac/unvac counters
        inf_counter[4,a] <- inf_counter[4,a] + vac_hist_mat[i,a]
        inf_counter[6,a] <- inf_counter[6,a] + (1-vac_hist_mat[i,a])
      # determine delta_v
        if(is.null(delta_v)) {
          if (v[i,a]>=999){
            delta_v <- 1
          } else {delta_v <- min(1,sum(drift[year_counter-v[i,a]:year_counter]))}
        }
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
      # update infection counters
        inf_counter[1,a] <- inf_counter[1,a] + inf_hist_mat[i,a]                           # total infections
        inf_counter[3,a] <- inf_counter[3,a] + (inf_hist_mat[i,a] * vac_hist_mat[i,a])     # vaccinated infections
        inf_counter[5,a] <- inf_counter[5,a] + (inf_hist_mat[i,a] * (1-vac_hist_mat[i,a])) # unvaccinated infections
      # update x and v
        if (a<maxage){
          x[i,a+1] <- x[i,a]+1
          v[i,a+1] <- v[i,a]+1
        }
      # update ages
        if (year_counter < years){
          ages[i] <- ages[i] + 1
          if (ages[i]==maxage){
            ages[i] <- 0
            inf_hist_mat[i,] <- c(0,rep(NA,maxage-1))
            vac_hist_mat[i,] <- c(0,rep(NA,maxage-1))
            suscept_mat[i,] <- c(1,rep(NA,maxage-1))
            x[i,] <- c(999,rep(NA,maxage-1))
            v[i,] <- c(999,rep(NA,maxage-1))

          }
        }
      # next person
          i <- i + 1
    } # end loop over individuals

  # calculate attack rate by age
    totals <- apply(inf_counter,1,sum)
    attack_rate[year_counter] <- totals[1]/totals[2]
    attack_rate_by_age[year_counter,] <- inf_counter[1,]/inf_counter[2,]
  # VE
    ve[year_counter] <- 1-((totals[3]/totals[4])/(totals[5]/totals[6]))
  # update vaccine update counter
    if (year_counter < years) {years_since_vac_update[year_counter + 1] <- years_since_vac_update[year_counter] + 1}
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
  ve_dat <- data.frame(Year=start_vac_year:end_year,VE=ve[(years-(end_year-start_vac_year)):years])

  # if (return_ar_only == 1){
  #   rtn <- dat
  # } else if (return_ar_only == 2){
  #   rtn <- attack_rate_by_age
  # } else {
    rtn <- list(history = init,
                attack_rate = dat,
                attack_rate_by_age = attack_rate_by_age,
                ve = ve_dat,
                inf_counter = inf_counter
                )
  # }

  return(rtn)
}
