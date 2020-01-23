#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function bootstraps the attack rates obtained from model simulations to obtain confidence intervals
#' @param dat data set
#' @param ages vector of ages
#' @param vac_strategy character string indicating vaccination strategy
#' @param stat statistic to be calculated for bootstrapping (mean or median)
#' @keywords morevac
#' @export

boot_ar <- function(dat, ages = 0:18, vac_strategy, stat = "median"){

if (stat == "median"){
  foo <- function(data, indices){
          dt<-data[indices,]
          c(apply(dt, 2, median, na.rm = TRUE))
  }
} else if (stat == "mean"){
  foo <- function(data, indices){
          dt<-data[indices,]
          c(apply(dt, 2, mean, na.rm = TRUE))
         }
} else {stop("stat must be either mean or median.")}

# bootstrap
myBootstrap <- boot(dat, foo, R=1000)

# create data set of original values and percentiles from bootstrapping
rows <- length(ages)
myAR <- data.frame(Age = ages, Vac_Strategy = c(rep(vac_strategy,rows)), Attack_Rate = myBootstrap$t0, Lower = numeric(rows), Upper = numeric(rows))

for (j in 1:rows){
  myAR[j,'Lower'] <- boot.ci(myBootstrap, index=j, type='perc')$percent[4]
  myAR[j,'Upper'] <- boot.ci(myBootstrap, index=j, type='perc')$percent[5]
}
return(myAR)
}
