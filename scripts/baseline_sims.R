### Baseline scenario script ###
# last modified: 22/01/2020

### preamble
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
library(rdist)
# load morevac package
# setwd("~/Documents/morevac") # Mac path
setwd("~/morevac") # PC path
devtools::load_all()
###

### define input parameters
file <- "C:/Users/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/baseline"

n_sim = 100
nindiv <- 30000
max_age = 80
myyears <- 1920:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 10
vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- 0.44
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- 0.44
#######################################
### run simulations
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

# extract cohorts from each sim and combine raw inf and vac histories for every simulation
sim0_results <- postprocess_sim_results_for_rolling_cohort(sim_dat = sim_test0, total_year_range = myyears, n_sim = n_sim)
sim1_results <- postprocess_sim_results_for_rolling_cohort(sim_dat = sim_test1, total_year_range = myyears, n_sim = n_sim)
sim2_results <- postprocess_sim_results_for_rolling_cohort(sim_dat = sim_test2, total_year_range = myyears, n_sim = n_sim)

# combine sim results into one data.table
inf_histories <- rbindlist(list(No_Vac = sim0_results$inf_history, Annual = sim1_results$inf_history, Biennial = sim2_results$inf_history), idcol = 'Vac_Strategy')
vac_histories <- rbindlist(list(No_Vac = sim0_results$vac_history, Annual = sim1_results$vac_history, Biennial = sim2_results$vac_history), idcol = 'Vac_Strategy')

# write raw output to file
try(data.table::fwrite(inf_histories, file = paste0(file,"_inf_hist.csv"), col.names = TRUE,
                       row.names = FALSE, sep = ","))
try(data.table::fwrite(vac_histories, file = paste0(file,"_vac_hist.csv"), col.names = TRUE,
                       row.names = FALSE, sep = ","))

#######################################
### read in results (rather than re-run simulations)
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
dt_inf <- vroom(file = "baseline/baseline_inf_hist.csv", delim = ",", col_names = TRUE) %>%
               mutate(Num_Infs = rowSums(select(.,Age0:Age18)))
dt_vac <- vroom(file = "baseline/baseline_vac_hist.csv", delim = ",", col_names = TRUE) %>%
               mutate(Num_Vacs = rowSums(select(.,Age0:Age18)))

dt_inf_16 <- vroom(file = "baseline/baseline_inf_hist_16.csv", delim = ",", col_names = TRUE) %>%
                mutate(Num_Infs = rowSums(select(.,Age0:Age18)))
dt_vac_16 <- vroom(file = "baseline/baseline_vac_hist_16.csv", delim = ",", col_names = TRUE) %>%
                mutate(Num_Vacs = rowSums(select(.,Age0:Age18)))

### summarise raw data for lifetime infections
banana <- bind_cols(dt_inf[,c("Vac_Strategy", "Sim", "Cohort", "ID", "Num_Infs")], Num_Vacs = dt_vac[,c("Num_Vacs")]) %>%
            group_by(Vac_Strategy, Sim) %>% summarise(Mean_Infs = mean(Num_Infs))

banana2 <- bind_cols(dt_inf_16[,c("Vac_Strategy", "Sim", "Cohort", "ID", "Num_Infs")], Num_Vacs = dt_vac_16[,c("Num_Vacs")]) %>%
            group_by(Vac_Strategy, Sim) %>% summarise(Mean_Infs = mean(Num_Infs))

# bootstrap to get CI for Lifetime Infs
foo1 <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Mean_Infs)
}
my_bootstrap <- plyr::dlply(banana, "Vac_Strategy", function(dat) boot(dat, foo1, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
banana_boat <- banana_boat %>% group_by(Vac_Strategy) %>% summarise(Mean_Infs = mean(Mean_Infs)) %>%
                mutate(Lower = my_ci[1,], Upper = my_ci[2,])

my_bootstrap <- plyr::dlply(banana2, "Vac_Strategy", function(dat) boot(dat, foo1, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
banana_boat2 <- banana_boat2 %>% group_by(Vac_Strategy) %>% summarise(Mean_Infs = mean(Mean_Infs)) %>%
                  mutate(Lower = my_ci[1,], Upper = my_ci[2,])

# Difference in Lifetime Infs
banana_split <- banana_boat %>% spread(Vac_Strategy, Mean_Infs) %>%
                  mutate(Diff_AB = Annual - Biennial, Diff_AN = Annual - No_Vac, Diff_BN = Biennial - No_Vac) %>%
                  select(Sim, Diff_AB, Diff_AN, Diff_BN) %>%
                  gather(Type, Difference, Diff_AB:Diff_BN)

# bootstrap to get CI for Difference
foo2 <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Difference)
}
my_bootstrap <- plyr::dlply(banana_split, "Type", function(dat) boot(dat, foo2, R=100)) # boostrap for each set of param values
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
                      mutate_at(vars(Age0:Age18), funs(./ID)) %>%
                      select(Vac_Strategy, Sim, Cohort, Age0:Age18) %>%
                      group_by(Vac_Strategy, Sim) %>%
                      summarise_at(vars(Age0:Age18), mean) %>%
                      gather(Age, Attack_Rate, Age0:Age18) %>%
                      mutate(Age = as.numeric(str_remove(Age, 'Age')))

# bootstrap to get CI for ARs
foo3 <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Attack_Rate)
}

my_bootstrap <- plyr::dlply(chocolate_sundae, c("Vac_Strategy","Age"), function(dat) boot(dat, foo3, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

chocolate_sundae2 <- chocolate_sundae %>% group_by(Vac_Strategy, Age) %>% summarise(Mean_AR = mean(Attack_Rate))
chocolate_sundae2$Lower <- my_ci[1,]
chocolate_sundae2$Upper <- my_ci[2,]
#######################################






