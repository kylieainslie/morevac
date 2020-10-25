### Latin Hypercube simulations script ###
# last modified: 07/10/2020

### preamble
# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(dplyr)
library(lhs)
library(boot)
library(data.table)
library(rdist)
library(vroom)
library(logitnorm)
# library(TruncatedDistributions)
# library(foreach)
# library(doParallel)

# load morevac package
# setwd("~/Documents/morevac") # Mac path
# setwd("~/morevac") # PC path
devtools::load_all()
###

#######################################
# specify save directory
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data")
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
# setwd("C:/Users/ainsl/Dropbox/Kylie/Projects/Morevac/data/sim_data/")

#######################################
 ### Baseline
#######################################
# run simulations
run_sims_all(params_file = "param_values_test.csv", out_file = "sim_test_")

#######################################
 ### Latin hypercube
#######################################
# create parameter combination
params <- create_params_file(n_sim = 1000, n_indiv = 30000, lhc_size = 250, out_file = "param_values_10",
                             vac_cutoff = 10, seed = 1234)

# run simulations
run_sims_all(params_file = "param_values_10.csv", index = 121:140, out_file = "sim_10_")

#######################################
 ### Simulation results post-processing
#######################################
# set working directory where results files are located
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10")
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/sim1000")

# read in results (rather than re-run simulations)
files_mean_infs <- list.files(pattern="mean_infs*")
mean_infs <- vroom(files_mean_infs)

files_mean_diff <- list.files(pattern="mean_diff*")
mean_diff <- vroom(files_mean_diff)

files_mean_ar <- list.files(pattern="mean_ar*")
mean_ar <- vroom(files_mean_ar)


############################################
### Fully vaccinated individuals
# vac_max <- c(max(banana[banana$Vac_Strategy == "Annual",]$Num_Vacs), max(banana[banana$Vac_Strategy == "Biennial",]$Num_Vacs))
# banana_pancake <- banana %>%
#   filter((Vac_Strategy == 'Annual' & Num_Vacs == vac_max[1]) |
#            (Vac_Strategy == 'Biennial' & Num_Vacs == vac_max[2])) %>%
#   group_by(Vac_Strategy, Param_Index, Sim) %>%
#   summarise(Mean_Infs = mean(Num_Infs)) %>%
#   spread(Vac_Strategy, Mean_Infs) %>%
#   mutate(Difference = Annual - Biennial) %>%
#   filter(!is.na(Difference))
#
# # bootstrap
# my_bootstrap2 <- plyr::dlply(banana_pancake, "Param_Index", function(dat) boot(dat, foo, R=1000)) # boostrap for each set of param values
# my_ci2 <- sapply(my_bootstrap2, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
# my_ci2[vapply(my_ci2, is.null, logical(1))] <- list(c(NA, NA))
# my_ci2a <- data.frame(matrix(unlist(my_ci2), nrow=length(my_ci2), byrow=T)) #bind_rows(my_ci2)
#
# banana_pancake2 <- banana_pancake %>% group_by(Param_Index) %>% summarise(Mean_Diff = mean(Difference))
# banana_pancake2$Lower <- my_ci2a[,1]
# banana_pancake2$Upper <- my_ci2a[,2]
# banana_pancake2 <- left_join(banana_pancake2, param_values, by = c("Param_Index"))
# # add Diff_Color column for plotting
# banana_pancake2$Diff_Color <- ifelse(banana_pancake2$Upper < 0, '<0',
#                                    ifelse(banana_pancake2$Lower <=0 & banana_pancake2$Upper >=0, 'zero',
#                                           ifelse(banana_pancake2$Lower >0, '>0', 'something else')))
# banana_pancake2$Diff_Color <- ifelse(is.na(banana_pancake2$Upper) & banana_pancake2$Mean_Diff <0, '<0', banana_pancake2$Diff_Color)
# banana_pancake2$Diff_Color <- ifelse(is.na(banana_pancake2$Lower) & banana_pancake2$Mean_Diff >0, '>0', banana_pancake2$Diff_Color)
# randomly select fully vac individuals and plot exposure history

