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
# determine vaccination coverages by age
vac_cov_dat <- data.frame(Age = 0:79,
                          No_Vaccination = c(rep(0,80)),
                          Annual_Constant_Vac = c(rep(0,2),rep(0.5,78)),           # vaccination coverage is 50% for everyone <= 2
                          Biannual_Constant_Vac = c(rep(0,2),rep(c(0.5,0),39)),
                          Annual_Total_Vac = c(rep(0,2),rep(1,78)),               # vaccination coverage is 100% for everyone <= 2
                          Biannual_Total_Vac = c(rep(0,2),rep(c(1,0),39)),
                          Annual_Off_At_10 = c(rep(0,2),rep(0.75,8),rep(0,8),rep(0.5,62)),
                          Biannual_Off_At_10 = c(rep(0,2),rep(c(0.75,0),4),rep(0,8),rep(c(0.5,0),31)),
                          Annual_Off_At_16 = c(rep(0,2),rep(0.75,14),rep(0,2),rep(0.5,62)),
                          Biannual_Off_At_16 = c(rep(0,2),rep(c(0.75,0),7),rep(0,2),rep(c(0.5,0),31))
                          )

# run multi-annual model
out <- multiannual(n=10000, years = 1820:2028, betas = c(0.4, rep(0.2,208)), vac_coverage = vac_cov_dat$Biannual_Constant_Vac, vac_strategy = 2)

# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat, ages_mat = out$ages, years = 1820:2028)
# plot total attack rates
p_out <- plot_attack_rates(dat = ar_out$attack_rates)
p_out

# isolate birth cohorts starting in 2000 (years 181:200)
my_cohorts <- get_cohorts(inf_history = out$inf_history$inf_hist_mat, vac_history = out$vac_history$vac_hist_mat, ages = out$ages,
                          total_year_range = 1820:2028)
cohort_sizes <- sapply(my_cohorts$cohort_ids, length)
ninfs <- sapply(my_cohorts$inf_hist,function(x) apply(x,2,sum))
colnames(ninfs) <- paste0("Cohort",1:10)
cohort_ar <- sweep(ninfs, 2, cohort_sizes, FUN="/")
avg_ar <- data.frame(Age = 0:18, Attack_Rate = apply(cohort_ar,1,mean))

# plot attack rates
p_cohort <- plot_attack_rates(dat = cohort_ar)
p_cohort

#######################################
### simulation
## create latin hypercube of parameter values to simulate over
set.seed(1234)
mylhc <- randomLHS(5, 6)
colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
mylhc[, "Epsilon"] <- qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05)

# log_test <- log(qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05), base = 10)
# log_test2 <- 10^quantile(log_test,probs = mylhc[,"Epsilon"])
## single run
setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/data")
for (i in 1:dim(mylhc)[1]){
# parameters
n_sim = 10
nindiv <- 30000
row_lhc <- i
max_age = 80
myyears <- 1820:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 10
vac_cov_none <- c(rep(0,max_age))
vac_cov_annual <- c(rep(0,2),rep(mylhc[row_lhc, "Vac_Cov"], vac_cut_off - 2), rep(0, max_age - vac_cut_off))
vac_cov_biennial <- c(rep(0,2),rep(c(mylhc[row_lhc, "Vac_Cov"], 0), vac_cut_off/2 - 1), rep(0, max_age - vac_cut_off))
# output note to user
cat("\n No vaccination simulation running... \n")
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_Vaccination, vac_strategy = 0,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Annual vaccination simulation running... \n")
sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_annual, vac_strategy = 1,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Every other year vaccination simulation running... \n")
sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_biennial, vac_strategy = 2,
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
   # c(median(dt[,1]),median(dt[,2]), median(dt[,3]),median(dt[,4]),
   #   median(dt[,5]),median(dt[,6]), median(dt[,7]),median(dt[,8]),
   #   median(dt[,9]),median(dt[,10]), median(dt[,11]),median(dt[,12]),
   #   median(dt[,13]),median(dt[,14]), median(dt[,15]),median(dt[,16]),
   #   median(dt[,17]),median(dt[,18]), median(dt[,19])
   # )
}

set.seed(12345)
myBootstrap0 <- boot(t(sim0_results$Attack_Rate), foo, R=1000)
myBootstrap1 <- boot(t(sim1_results$Attack_Rate), foo, R=1000)
myBootstrap2 <- boot(t(sim2_results$Attack_Rate), foo, R=1000)

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
   #c(median(dt[,3], na.rm = TRUE),median(dt[,4], na.rm = TRUE),
   #  median(dt[,5],na.rm = TRUE),median(dt[,6], na.rm = TRUE),
   #  median(dt[,7], na.rm = TRUE),median(dt[,8], na.rm = TRUE),
   #  median(dt[,9], na.rm = TRUE)
   #)
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
myLTI0[1,'Lower'] <- boot.ci(myBootstrap0, index=1, type='perc')$percent[4]
myLTI0[1,'Upper'] <- boot.ci(myBootstrap0, index=1, type='perc')$percent[5]
tryCatch({
for (k in 1:dim(myLTI1)[1]){
   # annual
   myLTI1[k,'Lower'] <- boot.ci(myBootstrap1, index=k, type='perc')$percent[4]
   myLTI1[k,'Upper'] <- boot.ci(myBootstrap1, index=k, type='perc')$percent[5]
   # biennial
   myLTI2[k,'Lower'] <- boot.ci(myBootstrap2, index=k, type='perc')$percent[4]
   myLTI2[k,'Upper'] <- boot.ci(myBootstrap2, index=k, type='perc')$percent[5]
}}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
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
write.csv(ar_out, file = "ar_sim_data.csv")
write.csv(li_out, file = "li_sim_data.csv")

