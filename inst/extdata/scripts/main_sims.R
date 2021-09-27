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

# load morevac package
# setwd("~/Documents/morevac") # Mac path
setwd("~/morevac") # PC path
devtools::load_all()
###

#######################################
# specify save directory
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data")
setwd("C:/Users/ainsliek/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
# setwd("C:/Users/ainsl/Dropbox/Kylie/Projects/Morevac/data/sim_data/")

#######################################
 ### Baseline
#######################################
# run simulations
run_sims_all(params_file = "param_values_baseline_16.csv", out_file = "sim_test_")

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
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10/sim1000")

# read in results (rather than re-run simulations)
files_mean_infs <- list.files(pattern="mean_infs*")
mean_infs <- vroom(files_mean_infs)
write.csv(mean_infs, "mean_infs_10_sim1000.csv")

files_mean_diff <- list.files(pattern="mean_diff*")
mean_diff <- vroom(files_mean_diff)
write.csv(mean_infs, "mean_diff_10_sim1000.csv")

files_mean_ar <- list.files(pattern="mean_ar*")
mean_ar <- vroom(files_mean_ar)
write.csv(mean_infs, "mean_ar_10_sim1000.csv")
