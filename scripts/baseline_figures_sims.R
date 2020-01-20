### Bootstrap simulation results ###

# preamble
# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(foreach)
library(doParallel)
library(dplyr)
library(lhs)
library(boot)
library(data.table)
# load morevac package
setwd("~/Documents/morevac")
devtools::load_all()

# parameters
n_sim = 100
nindiv <- 30000
max_age = 80
myyears <- 1820:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 10
vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- 0.44
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- 0.44

# output note to user
cat("\n No vaccination simulation running... \n")
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_vac, vac_strategy = 0,
                       wane = 1, take = 1, epsilon = 0, vac_protect = 0.7, rho = 0.9)
cat("\n Annual vaccination simulation running... \n")
sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Annual, vac_strategy = 1,
                       wane = 1, take = 1, epsilon = 0, vac_protect = 0.7, rho = 0.9)
cat("\n Every other year vaccination simulation running... \n")
sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Biennial, vac_strategy = 2,
                       wane = 1, take = 1, epsilon = 0, vac_protect = 0.7, rho = 0.9)

# combine sim results into one data.table
inf_histories <- rbindlist(list(No_Vac = sim0_results$inf_history, Annual = sim1_results$inf_history, Biennial = sim2_results$inf_history), idcol = 'Vac_Strategy')
vac_histories <- rbindlist(list(No_Vac = sim0_results$vac_history, Annual = sim1_results$vac_history, Biennial = sim2_results$vac_history), idcol = 'Vac_Strategy')

# write raw output to file
file <- "~/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/baseline"
try(data.table::fwrite(inf_histories, file = paste0(file,"_inf_hist.csv"), col.names = TRUE,
                       row.names = FALSE, sep = ","))
try(data.table::fwrite(vac_histories, file = paste0(file,"_vac_hist.csv"), col.names = TRUE,
                       row.names = FALSE, sep = ","))

#######################################
### read in results (rather than re-run simulations)
dt_inf <- fread("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/baseline_inf_hist.csv")
dt_vac <- fread("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/baseline_vac_hist.csv")

### summarise raw data
dt_inf1 <- dt_inf %>% mutate(Num_Infs = rowSums(select(.,Age0:Age18)))
dt_vac1 <- dt_vac %>% mutate(Num_Vacs = rowSums(select(.,Age0:Age18)))

banana1 <- dt_inf1 %>% group_by(Vac_Strategy, Sim, Cohort) %>% mutate(Tot_Infs = colSums(select(.,Age0:Age18)))
banana <- cbind(dt_inf1[,c("Vac_Strategy", "Sim", "Cohort", "ID", "Num_Infs")], Num_Vacs = dt_vac1[,c("Num_Vacs")])
banana_boat <- banana %>% group_by(Vac_Strategy, Sim) %>% summarise(Mean_Infs = mean(Num_Infs), Mean_AR = mean(colSums(select(.,Age0:Age18))/n()))
banana_split <- banana_boat %>% spread(Vac_Strategy, Mean_Infs)
banana_split$Difference <- banana_split$Annual - banana_split$Biennial

# bootstrap to get CI for Difference
foo <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Difference)
}
my_bootstrap <- plyr::dlply(banana_split, "Param_Index", function(dat) boot(dat, foo, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

banana_split2 <- banana_split %>% group_by(Param_Index) %>% summarise(Mean_Diff = mean(Difference))
banana_split2$Lower <- my_ci[1,]
banana_split2$Upper <- my_ci[2,]
banana_split2 <- left_join(banana_split2, param_values, by = c("Param_Index"))

# post process sim results
# sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = myyears, nsim = n_sim)
# sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = myyears, nsim = n_sim)
# sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = myyears, nsim = n_sim)

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

p_ar_baseline <- ggplot(data = dat, aes(x = Age, y = Attack_Rate, colour= Vac_Strategy)) +
                 geom_line() +
                 geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
                 xlab("Age (years)") +
                 ylab("Attack Rate") +
                 #labs(fill = "Vaccination Strategy") +
                 theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position = c(0.95, 0.95),
                      legend.justification = c("right", "top"),
                      legend.box.just = "right",
                      legend.margin = margin(6, 6, 6, 6),
                      legend.key = element_rect(fill = "white"))
p_ar_baseline

### lifetime infections
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
dat2 <- dat2[!is.na(dat2$Lifetime_Infs),] # remove NA rows
dat2$Vac_Status <- "None"
for(i in 1:dim(dat2)[1]){
  if (dat2$Vac_Strategy[i] == "Annual"){
    if (dat2$Num_Vacs[i] < 9 & dat2$Num_Vacs[i] > 0){dat2$Vac_Status[i] <- "Partially"
    } else if (dat2$Num_Vacs[i] == 9) {dat2$Vac_Status[i] <- "Fully" }
  }

  if (dat2$Vac_Strategy[i] == "Every Other Year"){
    if(dat2$Num_Vacs[i] < 5 & dat2$Num_Vacs[i] > 0){dat2$Vac_Status[i] <- "Partially"
    } else if (dat2$Num_Vacs[i] == 5){ dat2$Vac_Status[i] <- "Fully" }
  }
}
dat2$Vac_Status <- as.factor(dat2$Vac_Status)
dat2$Vac_Status <- factor(dat2$Vac_Status,levels(dat2$Vac_Status)[c(2,3,1)])
# take out partially vaccinated folks
dat2a <- dat2[dat2$Vac_Status!="Partially",]

p_li_baseline <- ggplot(data=dat2a, aes(x=Vac_Status, y=Lifetime_Infs, fill=Vac_Strategy)) +
                 geom_bar(stat="identity", color = "black", position=position_dodge(), width = 0.65) +
                 geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2, position=position_dodge(.9)) +
                 ylab('Number of Lifetime Infections') +
                 xlab("Vaccination Status") +
                 theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.position = c(0.95, 0.95),
                       legend.justification = c("right", "top"),
                       legend.box.just = "right",
                       legend.margin = margin(6, 6, 6, 6),
                       legend.key = element_rect(fill = "white"))
p_li_baseline
