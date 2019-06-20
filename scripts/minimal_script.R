# run code

# load required packages
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(cowplot)
  library(stringr)

# load morevac package
#  setwd("C:/Users/kainslie/Documents/GitHub/morevac")
#  setwd("~/Documents/morevac")
  devtools::load_all()

# run multi-annual model
  out <- multiannual2()

# outputs
  # plot attack rates
  p1 <- plot_attack_rates(dat = out$attack_rate)
  # plot lifetime infections
  inf_dat <- out$history[,,1]
  lifetime_inf <- get_lifetime_inf(inf_history = inf_dat, ages = 1:80, maxage = 80)

### checking version 2
  test <- multiannual2(vac_coverage = 0.25, suscept_func_version = 2)
  p_test <- plot_attack_rates(dat = test$attack_rate)
  inf_hist <- test$history[,,1]
  vac_hist <- test$history[,,2]
   suscept <- test$history[,,3]


birth_cohort <- which(rownames(vac_hist)==19)
bc_vac_hist <- vac_hist[birth_cohort,1:20]
bc_inf_hist <- inf_hist[birth_cohort,1:20]
bc_suscept <- suscept[birth_cohort,1:20]

bc_vac_hist[2,]
bc_inf_hist[2,]
bc_suscept[2,]
