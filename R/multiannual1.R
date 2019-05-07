### mulitannual model of infection and vaccination ###

#setwd("~/Google Drive/morevac")
setwd("C:/Users/kainslie/Google Drive/morevac")
source("R/morevac_functions.R")

library(ggplot2)

multiannual1 <- function(n=100, years=5,vac_threshold=3,vac_coverage=0.5,
                        mybeta=0.15,mydelta=0.2,mygamma=0.4,biannual=FALSE,
                        same_indiv = FALSE){
  init <- initialize_pop(nindiv=n,timepoints=years)
  suscept_mat <- init$susceptibility
  vac_hist_mat <- init$vac_history
  inf_hist_mat <- init$infection_matrix
  x <- init$x; v <- x
  attack_rate <- init$ar; vac_ar <- attack_rate; unvac_ar <- attack_rate
  incidence <- init$incidence
  
  if (same_indiv){
    vn <- sample(1:n,n*vac_coverage)
  }
  
  # start loop over years
  for (year in 1:years){
     for(i in 1:n){
      # generate random number for infection
      if (same_indiv){
        if (i %in% vn) {
          randnum_vac <- vac_coverage
        } else randnum_vac <- 1
        
      } else {
        randnum_vac <- runif(1,0,1)
      }
      # vaccinate
        if (randnum_vac <= vac_coverage & year >= vac_threshold){
          if (biannual == FALSE) {
            vac_hist_mat[i,year] <- 1
            v[i,year] <- 0
          } else if (biannual == TRUE & (year %% 2) != 0){
            vac_hist_mat[i,year] <- 1
            v[i,year] <- 0
          } else {vac_hist_mat[i,year] <- 0}
        } else {vac_hist_mat[i,year] <- 0}

        #print(vac_hist_mat[i,])
      # generate random number for infection
        randnum_inf <- runif(1,0,1)
      # calculate susceptibility function for person i
        #print(x[i,])
        #print(vac_hist_mat[i,])
        
        # never infected
        if (x[i,year] == 999){
          if (v[i,year] == 999 | v[i,year] > 1/mydelta){ # never vaccinated or vaccinated long enough ago for drift to have diminished protection
            suscept_mat[i,year] <- 1
          } else if (v[i,year]==0 | v[i,year]<=1/mydelta){ # vaccinated this year or within last few years 
            suscept_mat[i,year] <- (vac_hist_mat[i,year]*mygamma) + (v[i,year]*mydelta)
          } 
        }
        
        # infected and delta>0
        if (x[i,year]>=0 & x[i,year]<999 & mydelta>0){
          if (v[i,year]==999 | v[i,year] > 1/mydelta){ # never vaccinated
            if (x[i,year]< 1/mydelta){
              suscept_mat[i,year] <- x[i,year]*mydelta
            } else if (x[i,year]>= 1/mydelta) {
              suscept_mat[i,year] <- 1
            } 
          } else if (v[i,year]==0 | v[i,year]<=1/mydelta){ # vaccinated
            suscept_mat[i,year] <- min(x[i,year]*mydelta,(vac_hist_mat[i,year]*mygamma)+(v[i,year]*mydelta))
          } 
        }
        
        # infected and delta=0
        if (x[i,year]>=0 & x[i,year]<999 & mydelta==0){
          suscept_mat[i,year] <- 0
        }
        
      # infect person i if random number < beta*susceptability
        #print(suscept_mat[i,])
        if (randnum_inf<=mybeta*suscept_mat[i,year]) {
          inf_hist_mat[i,year] <- 1
          x[i,year] <- 0
        } else {inf_hist_mat[i,year] <- 0}
      # update x for the next year
        if (year<years){
          x[i,year+1] <- ifelse(x[i,year]<999,x[i,year]+1,x[i,year])
          v[i,year+1] <- ifelse(v[i,year]<999,v[i,year]+1,v[i,year])
        }
    }
    
    # calculate attack rate
    attack_rate[year] <- sum(inf_hist_mat[,year])/n
    vac <- which(vac_hist_mat[,year]==1)
    unvac <- which(vac_hist_mat[,year]==0)
    vac_ar[year] <- sum(inf_hist_mat[vac,year])/n
    unvac_ar[year] <- sum(inf_hist_mat[unvac,year])/n
    incidence[year] <- sum(inf_hist_mat[,year])
  }
  rtn <- list(infection_matrix=inf_hist_mat,x=x,vac_history=vac_hist_mat,
              susceptibility=suscept_mat,attack_rate=attack_rate,incidence=incidence)
  dat <- data.frame(Year=1:years,Attack_Rate=attack_rate,Incidence=incidence)
  dat2 <- data.frame(Year=rep(1:years,2),Attack_Rate=c(vac_ar,unvac_ar),Vac_Status=c(rep("Vaccinated",years),rep("Unvaccinated",years)))
  dat2 <-dat2[order(dat2$Year),]
  p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate)) +       
        geom_line()
  
  p2 <- ggplot(data = dat2, aes(x = Year, y = Attack_Rate, colour= Vac_Status)) +       
        geom_line() +
        geom_vline(xintercept=2, linetype="dashed", color = "black") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6),
              legend.key = element_rect(fill = "white")
        )
  
  return(list(rtn,p1,p2))
}
