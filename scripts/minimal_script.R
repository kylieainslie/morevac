# run code

# load required packages
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(cowplot)
  library(stringr)
# load morevac package
  setwd("C:/Users/kainslie/Google Drive/morevac")
# setwd("~/Google Drive/morevac")
  devtools::load_all()

# run multi-annual model
  # either-or susceptibility version
    out1a <- multiannual2()
    out1b <- multiannual2(biannual = TRUE)
  # multiplicative susceptibility version
    out2a <- multiannual2(suscept_func_version = 2)
    out2a <- multiannual2(biannual = TRUE, suscept_func_version = 2)
  # correlation of vaccination version
    out2a <- multiannual2(rho = 0.9)
    out2a <- multiannual2(biannual = TRUE, rho = 0.9)

# outputs
  p1 <- plot_attack_rates(dat = out[[1]]$attack_rate)
#  p_age <- plot_attack_rates(dat = out[[1]]$attack_rate_by_age, by = '')

# run simulations
  sim_out <- run_sim(sim = 10,nindiv = 5000,year_range = c(2000:2019),
                     age_range = c(0:19),vaccov = 0.5,filename = "figures/corr_vac_0.9",
                     version = 1, rho = 0.9)
