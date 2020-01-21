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

### summarise raw data for lifetime infections
dt_inf1 <- dt_inf %>% mutate(Num_Infs = rowSums(select(.,Age0:Age18)))
dt_vac1 <- dt_vac %>% mutate(Num_Vacs = rowSums(select(.,Age0:Age18)))

banana <- cbind(dt_inf1[,c("Vac_Strategy", "Sim", "Cohort", "ID", "Num_Infs")], Num_Vacs = dt_vac1[,c("Num_Vacs")])
banana_boat <- banana %>% group_by(Vac_Strategy, Sim) %>% summarise(Mean_Infs = mean(Num_Infs))

# bootstrap to get CI for Lifetime Infs
foo <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Mean_Infs)
}
my_bootstrap <- plyr::dlply(banana_boat, "Vac_Strategy", function(dat) boot(dat, foo, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
banana_boat2 <- banana_boat %>% group_by(Vac_Strategy) %>% summarise(Mean_Infs = mean(Mean_Infs))
banana_boat2$Lower <- my_ci[1,]
banana_boat2$Upper <- my_ci[2,]

# Difference in Lifetime Infs
banana_split <- banana_boat %>% spread(Vac_Strategy, Mean_Infs) %>%
                  mutate(Diff_AB = Annual - Biennial, Diff_AN = Annual - No_Vac, Diff_BN = Biennial - No_Vac) %>%
                  select(Sim, Diff_AB, Diff_AN, Diff_BN) %>%
                  gather(Type, Difference, Diff_AB:Diff_BN)

# bootstrap to get CI for Difference
foo <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Difference)
}
my_bootstrap <- plyr::dlply(banana_split, "Type", function(dat) boot(dat, foo, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

banana_split2 <- banana_split %>% group_by(Type) %>% summarise(Mean_Diff = mean(Difference))
banana_split2$Lower <- my_ci[1,]
banana_split2$Upper <- my_ci[2,]

### summarise raw data for attack rates
chocolate <- dt_inf %>%
              group_by(Vac_Strategy, Sim, Cohort) %>%
              select(-ID) %>%
              summarise_all(list(sum))
chocolate_sprinkles <- dt_inf1 %>%
                        group_by(Vac_Strategy, Sim, Cohort) %>%
                        do(tail(.,1))
chocolate$ID <- chocolate_sprinkles$ID
chocolate_sundae <- chocolate %>%
                      mutate(AR0 = Age0/ID, AR1 = Age1/ID, AR2 = Age2/ID, AR3 = Age3/ID,
                             AR4 = Age4/ID, AR5 = Age5/ID, AR6 = Age6/ID, AR7 = Age7/ID,
                             AR8 = Age8/ID, AR9 = Age9/ID, AR10 = Age10/ID, AR11 = Age11/ID,
                             AR12 = Age12/ID, AR13 = Age13/ID, AR14 = Age14/ID, AR15 = Age15/ID,
                             AR16 = Age16/ID, AR17 = Age17/ID, AR18 = Age18/ID) %>%
                      select(Vac_Strategy, Sim, Cohort, AR0:AR18) %>%
                      group_by(Vac_Strategy, Sim) %>%
                      summarise_at(vars(AR0:AR18), mean) %>%
                      gather(Age, Attack_Rate, AR0:AR18) %>%
                      mutate(Age = as.numeric(str_remove(Age, 'AR')))

# bootstrap to get CI for ARs
foo <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Attack_Rate)
}

my_bootstrap <- plyr::dlply(chocolate_sundae, c("Vac_Strategy","Age"), function(dat) boot(dat, foo, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

chocolate_sundae2 <- chocolate_sundae %>% group_by(Vac_Strategy, Age) %>% summarise(Mean_AR = mean(Attack_Rate))
chocolate_sundae2$Lower <- my_ci[1,]
chocolate_sundae2$Upper <- my_ci[2,]

### plots
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
