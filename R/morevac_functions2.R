### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

    
#' Initialize population
#'
#' This function initializes the population before running the model.
#' @param nindiv Number of individuals in the simulated population. Defaults to 1000.
#' @param years Number of timepoints to simulate over. Defaults to 10.
#' @return List containing initialized infection history matrix, vaccination history matrix, susceptibility matrix, attack rate vector, and incidence vector.
#' @keywords morevac
#' @export
#' @examples
#' initialize_pop()    
initialize_pop <- function(nindiv=1000,maxage=80,years=200){
  # infection status matrix
    inf_stat_mat <- array(NA,dim=c(nindiv,maxage,years))
    colnames(inf_stat_mat) <- c(paste0("Age",0:(maxage-1)))
  # years since last infection matrix  
    x <- matrix(c(rep(999, maxage*nindiv)),nrow=nindiv)
    colnames(x) <- c(paste0("Age",0:(maxage-1)))
    
  # vaccination history matrix
    vac_hist_mat <- array(NA,dim=c(nindiv,maxage,years))
    colnames(vac_hist_mat) <- c(paste0("Age",0:(maxage-1)))
  # susceptibility matrix
    suscept_mat <- array(1,dim=c(nindiv,maxage,years))
    colnames(suscept_mat) <- c(paste0("Age",0:(maxage-1)))
  # ages
    ages <- sample(0:(maxage-1),nindiv,replace = TRUE)
  # attack rate  
    attack_rate <- c(rep(NA,years))
    #incidence <- c(rep(NA,years))
    
    return(list(infection_matrix=inf_stat_mat,x=x,vac_history=vac_hist_mat,
                susceptibility=suscept_mat,attack_rate=attack_rate, age=ages))
}
### susceptibility function
susceptibility_function <- function(x=999,v=0,delta=0.2,mygamma=0.4){
  #if(!is.integer(x)){stop("x must be an integer")}
  if(delta<0 | delta>1){stop("delta must be between 0 and 1 (inclusive)")}
  if(mygamma<0 | mygamma>1){stop("mygamma must be between 0 and 1 (inclusive)")}
      
  if(x==0){rtn <- 0}
  else if(x>=1 & x<999 & delta==1){rtn <- 0}
  else if(x>0 & x<(1/delta)) {rtn <- max(x*delta,v*mygamma)}
  else {rtn <- 1}
      
  return(rtn)
}

# calculate susceptibility
calc_susceptibility <-function(suscept_mat,x,vac_hist_mat, year, nindiv,delta=0.2,mygamma=0.4){
  
  for(i in 1:nindiv){
    suscept_mat[i,year] <- susceptibility_function(x=x[i,year],v=vac_hist_mat[i,year])
  }
  return(suscept_mat[,year])
}
  
### infection probability function
# a person's probability of infection depends on their susceptibility
    
infect <- function(mymybeta=0.15,suscept_mat,year,inf_hist_mat,lastinf){
  n <- dim(suscept_mat)[1]
  randnum <-runif(n,0,1)
  for (i in 1:nindiv){
    if (randnum[i]<=mymybeta*suscept_mat[i,year]) {
      inf_hist_mat[i,year] <- 1
      lastinf[i,year] <- 0
    }
    else {inf_hist_mat[i,year] <- 0}
  }
  
  return(list(inf_hist_mat[,year],lastinf))    
}

### vaccination function

vaccinate_annual <- function(vac_hist_mat,vac_coverage=0.5,year,nindiv,same=FALSE){
  randnum <-runif(nindiv,0,1)
  #if(same){ # vaccinate same individuals every year
  #  
  #}
  vac_hist_mat[,year] <- ifelse(randnum<vac_coverage,1,0)
  return(vac_hist_mat[,year])
}

vaccinate_biannual <- function(vac_hist_mat,vac_coverage=0.5,year,nindiv){
  randnum <-runif(nindiv,0,1)
  tovac <- which(vac_hist_mat[,(year-1)]==0) # only vaccinate people who weren't vaccinated last year
  vac_hist_mat[tovac,year] <- ifelse(randnum[tovac]<vac_coverage,1,0)
  return(vac_hist_mat[,year])
}
### calculate attack rate
calculate_ar <- function(inf_hist_mat,nindiv,year){
  attack_rate <- sum(inf_hist_mat[,year])/nindiv
  # incidence <- sum(inf_hist_mat[,year])
  return(ar)
}
### Run simulation

nindiv <- 100       # number of individuals
bins <- 1           # time unit: 1=annual, 2=semi-annual, 4=quarterly, etc.
startyear <-2010    # start year of simulation
endyear <- 2018     # end year of simulation
years <- (endyear-startyear)*bins
vac_threshold <- 3

# start simulation loop
run_sim <- function(n=100, years=timepoints,vac_threshold=3,vac_coverage=0.5,mybeta=0.15,delta=0.2,mygamma=0.4,biannual=FALSE){
  init <- initialize_pop(nindiv=n,timepoints=years)
  suscept_mat <- init$susceptibility
  vac_hist_mat <- init$vac_history
  inf_hist_mat <- init$infection_matrix
  x <- init$x
  for (t in 1:years){
  
    if (t >= vac_threshold){ 
      vac_hist_mat[,t] <- vaccinate_annual(vac_hist_mat,year=t,nindiv=n)
    }
    if (t<vac_threshold){
      vac_hist_mat[,t] <- c(rep(0,n))
    }
    
    suscept_mat[,t] <- calc_susceptibility(suscept_mat,x,vac_hist_mat,year=t,nindiv=n)
  
    infections <- infect(mybeta, suscept_mat, year=t, inf_hist_mat,lastinf = x)
    inf_hist_mat[,t] <- infections[[1]]
    x <- infections[[2]]
    # update x for next year
    if (t<years){
      x[,t+1] <- ifelse(x[,t]<999,x[,t]+1,x[,t])
    }
    # calculate attack rate
    attack_rate[t] <- calculate_ar(inf_hist_mat,n,year=t)
  }
  rtn <- list(infection_matrix=inf_hist_mat,x=x,vac_history=vac_hist_mat,susceptibility=suscept_mat,ar=ar)
  return(rtn)
}

#out <- run_sim(n=1000, years = 20)

