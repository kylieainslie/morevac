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
#' @param biannual logical. annual or biannual vaccination?
#' @return list with two elements: 1) a list of infection histories and attack rates and
#'         2) a plot of annual attack rates by vaccination scenario
#' @keywords morevac
#' @export
multiannual2 <- function(n = 100,
                         years = 200,
                         maxage = 80,
                         start_year = 1820,
                         start_vac_year = 2000,
                         start_vac_age = 3,
                         vac_coverage = 0.5,
                         beta_pandemic = 0.4,
                         beta_epidemic = 0.2,
                         delta_x = 0.2,
                         delta_v = 0.2,
                         mygamma = 0.4,
                         biannual = FALSE
                         ){

  init <- initialize_pop(nindiv = n, years = years, maxage = maxage)
  x <- init$x; v <- x
  attack_rate <- init$ar
  attack_rate_by_age <- init$attack_rate_by_age
  ages <- init$age
  #print(ages)
  # select matrix in array for current year
     suscept_mat <- init$susceptibility[,,1]
    vac_hist_mat <- init$vac_history[,,1]
    inf_hist_mat <- init$infection_matrix[,,1]
    lifetime_inf <- init$lifetime_infections[,,1]
  #if (same_indiv){
  #  vn <- sample(1:n,n*vc)
  #}

  # year counter
    sim_year <- 1
    actual_year <- start_year
  # start loop over years
  for (year_counter in 1:years){
    #print(year_counter)
    #print(lifetime_inf)
    inf_counter <- matrix(c(rep(0,maxage*2)),nrow=2)
    rownames(inf_counter) <- c('number_of_infections','n_age')
    colnames(inf_counter) <- c(paste0("Age",0:(maxage-1)))
    #print(dim(inf_counter))
    # turn off vaccination until start_vac_year
    if (actual_year<start_vac_year){vc <- 0
    } else {vc <- vac_coverage}

    # for first year have pandemic transmission rate
    if (year_counter==1){
      mybeta <-beta_pandemic
    } else {mybeta <- beta_epidemic}

    # loop over individuals
    for(i in 1:n){
      #print(i)
      a <- ages[i] + 1
      #print(a)

      inf_counter[2,a] <- inf_counter[2,a] + 1
      # determine who will be vaccinated
        if (actual_year >= start_vac_year & ages[i] >= start_vac_age){
            randnum_vac <- runif(1,0,1)
            # vaccinate
            if (randnum_vac <= vc & actual_year >= start_vac_year){
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

      # generate random number for infection
        randnum_inf <- runif(1,0,1)

      # calculate susceptibility function for person i
        if(is.na(x[i,a])){x[i,a]<-999}

        # never infected
        #print(x[i,a])
        if (x[i,a] == 999){
          if (v[i,a] == 999 | v[i,a] > 1/delta_v){ # never vaccinated or vaccinated long enough ago for drift to have diminished protection
            suscept_mat[i,a] <- 1
          } else if (v[i,a]==0 | v[i,a]<=1/delta_v){ # vaccinated this year or within last few year_counters
            suscept_mat[i,a] <- (vac_hist_mat[i,a]*mygamma) + (v[i,a]*delta_v)
          }
        }

        # infected and delta>0
        if (x[i,a]>=0 & x[i,a]<999 & delta_x>0 & delta_v>0){
          if (v[i,a]==999 | v[i,a] > 1/delta_v){ # never vaccinated
            if (x[i,a]< 1/delta_x){
              suscept_mat[i,a] <- x[i,a]*delta_x
            } else if (x[i,a]>= 1/delta_x) {
              suscept_mat[i,a] <- 1
            }
          } else if (v[i,a]==0 | v[i,a]<=1/delta_v){ # vaccinated
            suscept_mat[i,a] <- min(x[i,a]*delta_x,(vac_hist_mat[i,a]*mygamma)+(v[i,a]*delta_v))
          }
        }

        # infected and delta=0
        if (x[i,a]>=0 & x[i,a]<999 & delta_x==0 & delta_v==0){
          suscept_mat[i,a] <- 0
        }

      # infect person i if random number < beta*susceptability
        #print(suscept_mat[i,])
        if (randnum_inf<=mybeta*suscept_mat[i,a]) {
          inf_hist_mat[i,a] <- 1
          x[i,a] <- 0
          inf_counter[1,a] <- inf_counter[1,a] + 1
          lifetime_inf[i,a] <- ifelse(a>1,lifetime_inf[i,a-1] + 1, 1)
        } else {
            inf_hist_mat[i,a] <- 0
            lifetime_inf[i,a] <- ifelse(a>1,lifetime_inf[i,a-1], 0)
          }

      # update individual counters
        #if (year_counter<years){
        #  if (x[i,a] == 999){
        #    x[i,a+1] <- 999
        #  } else if (x[i,a]<999){
        #    x[i,a+1] <- x[i,a]+1
        #  }
        if (a<maxage){
          x[i,a+1] <- ifelse(x[i,a]<999,x[i,a]+1,999)
          v[i,a+1] <- ifelse(v[i,a]<999,v[i,a]+1,999)
        }
        ages[i] <- ages[i] + 1
        if (ages[i]==80){ages[i] <- 0}
    } # end loop over individuals

  # calculate attack rate by age
    attack_rate[year_counter] <- sum(inf_counter[1,])/sum(inf_counter[2,])
    attack_rate_by_age[year_counter,] <- inf_counter[1,]/inf_counter[2,]
  # store current year's inf, vac, suscept in array
    init$infection_matrix[,,year_counter] <- inf_hist_mat
    init$vac_history[,,year_counter] <- vac_hist_mat
    init$susceptibility[,,year_counter] <- suscept_mat
    init$lifetime_infections[,,year_counter] <- lifetime_inf
  # reset history for new naive individuals
    inf_hist_mat[which(ages==0),] <- c(0,rep(NA,maxage-1))
    vac_hist_mat[which(ages==0),] <- c(0,rep(NA,maxage-1))
     suscept_mat[which(ages==0),] <- c(1,rep(NA,maxage-1))
               x[which(ages==0),] <- c(999,rep(NA,maxage-1))
    lifetime_inf[which(ages==0),] <- c(0,rep(NA,maxage-1))
  # update counters
    sim_year <- sim_year + 1
    actual_year <- actual_year + 1
 } # end loop over years
# output
  end_year <- start_year + years - 1
  dat <- data.frame(Year=start_year:end_year,Attack_Rate=attack_rate)
  mean_ar <- mean(dat[dat$Year%in% start_year:(start_vac_year-1),2])
  #cat('Mean attack rate prior to vaccination:',mean_ar,'\n')
  rownames(attack_rate_by_age) <- start_year:end_year

  rtn <- list(history=init,
              attack_rate=dat,
              attack_rate_by_age = attack_rate_by_age
              )
# plots
  p1 <- plot_attack_rates(dat, y_max = 0.5)


  return(list(rtn,p1))
}
