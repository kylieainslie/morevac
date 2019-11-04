# rolling cohort script

# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(foreach)
library(doParallel)
library(dplyr)
library(plyr)
library(lhs)
library(boot)
# load morevac package
setwd("~/Documents/morevac")
devtools::load_all()
#######################################
### simulation
## create latin hypercube of parameter values to simulate over
set.seed(1234)
#set.seed(5678)
mylhc <- randomLHS(500, 6)
colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
mylhc[, "Epsilon"] <- qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05)
mylhc[, "Vac_Cov"] <- qunif(mylhc[,"Vac_Cov"], min = 0.301, max = 0.806) # range from 2018-2019 PHE estimates of VC in school-aged children: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/806289/Childhood_flu_annual_report_2018_19_FINAL_.pdf
mylhc[, "Take"] <- qunif(mylhc[,"Take"], min = 0.5, max = 1)
# log_test <- log(qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05), base = 10)
# log_test2 <- 10^quantile(log_test,probs = mylhc[,"Epsilon"])
## single run
setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/data")
# parallelize
bins <- list(seq(1,100),seq(101,200),seq(201,300),seq(301,400),seq(401,500))
# make cluster
# ncl <- detectCores()
# cl <- makeCluster(ncl)
# registerDoParallel(cl)
for (b in 1:5){
#foreach(j=1:5, .packages = c('morevac','Rcpp')) %dopar% {

loop_length <- length(bins[[b]])
for (i in 1:loop_length){
cat("\n Simulation ",i," of",loop_length,"\n")
# parameters
n_sim = 5
nindiv <- 30000
row_lhc <- bins[[b]][i]
max_age = 80
myyears <- 1820:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 10
vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- mylhc[row_lhc,"Vac_Cov"]
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- mylhc[row_lhc,"Vac_Cov"]
# output note to user
cat("\n No vaccination simulation running... \n")
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_Vac, vac_strategy = 0,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Annual vaccination simulation running... \n")
sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Annual, vac_strategy = 1,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Every other year vaccination simulation running... \n")
sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Biennial, vac_strategy = 2,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])

# post process sim results
sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = myyears, nsim = n_sim)
sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = myyears, nsim = n_sim)
sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = myyears, nsim = n_sim)

cat("\n Bootstrapping... \n")
# bootstrapping
foo <- function(data, indices){
   dt<-data[indices,]
   c(apply(dt, 2, median, na.rm = TRUE))
}

myBootstrap0 <- boot(sim0_results$Attack_Rate, foo, R=1000)
myBootstrap1 <- boot(sim1_results$Attack_Rate, foo, R=1000)
myBootstrap2 <- boot(sim2_results$Attack_Rate, foo, R=1000)

# create data set of original medians and percentiles from bootstrapping
myAR0 <- data.frame(Age= 0:18, Vac_Strategy = c(rep('No Vaccination',19)), Attack_Rate = myBootstrap0$t0, Lower = numeric(19), Upper = numeric(19))
myAR1 <- data.frame(Age= 0:18, Vac_Strategy = c(rep('Annual',19)), Attack_Rate = myBootstrap1$t0, Lower = numeric(19), Upper = numeric(19))
myAR2 <- data.frame(Age= 0:18, Vac_Strategy = c(rep('Every Other Year',19)), Attack_Rate = myBootstrap2$t0, Lower = numeric(19), Upper = numeric(19))

for (j in 1:dim(myAR0)[1]){
# no vaccination
   myAR0[j,'Lower'] <- boot.ci(myBootstrap0, index=j, type='perc')$percent[4]
   myAR0[j,'Upper'] <- boot.ci(myBootstrap0, index=j, type='perc')$percent[5]
# annual
   myAR1[j,'Lower'] <- boot.ci(myBootstrap1, index=j, type='perc')$percent[4]
   myAR1[j,'Upper'] <- boot.ci(myBootstrap1, index=j, type='perc')$percent[5]
# biennial
   myAR2[j,'Lower'] <- boot.ci(myBootstrap2, index=j, type='perc')$percent[4]
   myAR2[j,'Upper'] <- boot.ci(myBootstrap2, index=j, type='perc')$percent[5]

}
# combine AR sim results into single data set to plot
dat <- rbind(myAR0, myAR1, myAR2)
dat$Vac_Cov <- mylhc[row_lhc, "Vac_Cov"]
dat$Waning <- mylhc[row_lhc, "Waning"]
dat$Take = mylhc[row_lhc, "Take"]
dat$Epsilon = mylhc[row_lhc, "Epsilon"]
dat$Rho = mylhc[row_lhc, "Rho"]
dat$VE = mylhc[row_lhc, "VE"]

if (i == 1){ ar_out <- dat
} else {ar_out <- rbind(ar_out, dat)}

# post-post-processing of lifetime infections
lifetime_infs0 <- data.frame(sim0_results$Lifetime_Infections, Vac_Strategy = c(rep("No Vaccination",dim(sim0_results$Lifetime_Infections)[1])))
lifetime_infs1 <- data.frame(sim1_results$Lifetime_Infections, Vac_Strategy = c(rep("Annual",dim(sim1_results$Lifetime_Infections)[1])))
lifetime_infs2 <- data.frame(sim2_results$Lifetime_Infections, Vac_Strategy = c(rep("Every Other Year",dim(sim2_results$Lifetime_Infections)[1])))

# wide format
li0_wide <- spread(lifetime_infs0, Num_Vacs, med)
li1_wide <- spread(lifetime_infs1, Num_Vacs, med)
li2_wide <- spread(lifetime_infs2, Num_Vacs, med)
all_li_wide <- rbind.fill(li0_wide, li1_wide, li2_wide)

# bootstrapping
foo2 <- function(data, indices){
   dt<-data[indices,]
   c(apply(dt[,-c(1,2)], 2, median, na.rm = TRUE))
}

#set.seed(12345)
myBootstrap0 <- boot(all_li_wide[all_li_wide$Vac_Strategy == 'No Vaccination',], foo2, R=1000)
myBootstrap1 <- boot(all_li_wide[all_li_wide$Vac_Strategy == 'Annual',], foo2, R=1000)
myBootstrap2 <- boot(all_li_wide[all_li_wide$Vac_Strategy == 'Every Other Year',], foo2, R=1000)
num_cols <- length(myBootstrap1$t0)
# create data set of original medians and percentiles from bootstrapping
myLTI0 <- data.frame(Num_Vacs= 0:(num_cols-1), Vac_Strategy = c(rep('No Vaccination',num_cols)), Lifetime_Infs = myBootstrap0$t0, Lower = rep(NA,num_cols), Upper = rep(NA,num_cols))
myLTI1 <- data.frame(Num_Vacs= 0:(num_cols-1), Vac_Strategy = c(rep('Annual',num_cols)), Lifetime_Infs = myBootstrap1$t0, Lower = rep(NA,num_cols), Upper = rep(NA,num_cols))
myLTI2 <- data.frame(Num_Vacs= 0:(num_cols-1), Vac_Strategy = c(rep('Every Other Year',num_cols)), Lifetime_Infs = myBootstrap2$t0, Lower = rep(NA,num_cols), Upper = rep(NA,num_cols))

# no vaccination
lower.ci <- boot.ci(myBootstrap0, index=1, type='perc')$percent[4]
upper.ci<- boot.ci(myBootstrap0, index=1, type='perc')$percent[5]
myLTI0[1,'Lower'] <- ifelse (is.null(lower.ci), NA, lower.ci)
myLTI0[1,'Upper'] <- ifelse (is.null(upper.ci), NA, upper.ci)

tryCatch({ # don't stop for loop if there is an error in calculating bootstrap CIs
   for (k in 1:dim(myLTI1)[1]){
      print(k)
      # annual
      a.lower.ci <- boot.ci(myBootstrap1, index=k, type='perc')$percent[4]
      a.upper.ci <-  boot.ci(myBootstrap1, index=k, type='perc')$percent[5]
      myLTI1[k,'Lower'] <- ifelse (is.null(a.lower.ci), NA, a.lower.ci)
      myLTI1[k,'Upper'] <- ifelse (is.null(a.upper.ci), NA, a.upper.ci)
      # biennial
      if(sum(myBootstrap2$t[,k], na.rm = TRUE) > 0){
         b.lower.ci <- boot.ci(myBootstrap2, index=k, type='perc')$percent[4]
         b.upper.ci <-  boot.ci(myBootstrap2, index=k, type='perc')$percent[5]
         myLTI2[k,'Lower'] <- ifelse (is.null(b.lower.ci), NA, b.lower.ci)
         myLTI2[k,'Upper'] <- ifelse (is.null(b.upper.ci), NA, b.upper.ci)
      } else {next}
   }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# combine AR sim results into single data set to plot
dat2 <- rbind(myLTI0, myLTI1, myLTI2)
dat2$Vac_Cov <- mylhc[row_lhc, "Vac_Cov"]
dat2$Waning <- mylhc[row_lhc, "Waning"]
dat2$Take = mylhc[row_lhc, "Take"]
dat2$Epsilon = mylhc[row_lhc, "Epsilon"]
dat2$Rho = mylhc[row_lhc, "Rho"]
dat2$VE = mylhc[row_lhc, "VE"]

if(i == 1){li_out <- dat2
} else {li_out <- rbind.fill(li_out, dat2)}
}
cat("\n Complete!")
# write results to file
write.csv(ar_out, file = paste0("attack_rates/ar_sim_data_",b,".csv"))
write.csv(li_out, file = paste0("lifetime_infs/li_sim_data_",b,".csv"))
} # end b loop
# } # end foreach

# stop cluster
# stopCluster(cl)
