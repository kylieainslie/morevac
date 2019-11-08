#' Multi-annual model of infection and vaccination (version 2)
#'
#' This function bootstraps the number of lifetime infections obtained from model simulations to obtain confidence intervals
#' @param dat data set
#' @param ages vector of ages
#' @param vac_strategy character string indicating vaccination strategy
#' @param stat statistic to be calculated for bootstrapping (mean or median)
#' @keywords morevac
#' @export

boot_li <- function(dat, ages = 0:18, vac_strategy, stat = "median"){

  tmp <- data.frame(dat, Vac_Strategy = c(rep(vac_strategy,dim(dat)[1])))

  if (stat == "median"){
    tmp_wide <- spread(tmp[,-3], Num_Vacs, med)

    foo <- function(data, indices){
      dt<-data[indices,]
      c(apply(dt[,-c(1,2)], 2, median, na.rm = TRUE))
    }
  } else if (stat == "mean"){
    tmp_wide <- spread(tmp[,-2], Num_Vacs, mean)

    foo <- function(data, indices){
      dt<-data[indices,]
      c(apply(dt[,-c(1,2)], 2, mean, na.rm = TRUE))
    }
  } else {stop("stat must be either mean or median.")}

  # bootstrap
  if(ncol(tmp_wide) == 3){
    tmp_wide$dummy <- tmp_wide$`0` # add dummy column
    myBootstrap <- boot(tmp_wide, foo, R=1000)
    myBootstrap$t0 <- myBootstrap$t0[-2]  # remove dummy value
  } else {myBootstrap <- boot(tmp_wide, foo, R=1000)}
  # create data set of original values and percentiles from bootstrapping
  num_cols <- length(myBootstrap$t0)
  myLI <- data.frame(Num_Vacs= 0:(num_cols-1), Vac_Strategy = c(rep(vac_strategy,num_cols)), Lifetime_Infs = myBootstrap$t0, Lower = rep(NA,num_cols), Upper = rep(NA,num_cols))

  if (vac_strategy == "No Vaccination"){
    lower.ci <- boot.ci(myBootstrap, index=1, type='perc')$percent[4]
    upper.ci<- boot.ci(myBootstrap, index=1, type='perc')$percent[5]
    myLI[1,'Lower'] <- ifelse (is.null(lower.ci), NA, lower.ci)
    myLI[1,'Upper'] <- ifelse (is.null(upper.ci), NA, upper.ci)
  } else{
      tryCatch({ # don't stop for loop if there is an error in calculating bootstrap CIs
        for (k in 1:num_cols){
          lower.ci <- boot.ci(myBootstrap, index=k, type='perc')$percent[4]
          upper.ci <- boot.ci(myBootstrap, index=k, type='perc')$percent[5]
          myLI[k,'Lower'] <- ifelse (is.null(lower.ci), NA, lower.ci)
          myLI[k,'Upper'] <- ifelse (is.null(upper.ci), NA, upper.ci)
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
  # output
  return(myLI)
}

