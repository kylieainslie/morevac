# run code

# load required packages
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(cowplot)
  library(stringr)

# load morevac package
  setwd("C:/Users/kainslie/Google Drive/morevac")
# setwd("~/Documents/morevac")
  devtools::load_all()

# run multi-annual model
  out <- multiannual2()

# outputs
  # plot attack rates
  p1 <- plot_attack_rates(dat = out$attack_rate)
  # plot lifetime infections
  inf_dat <- out$history[,,1]
  lifetime_inf <- get_lifetime_inf(inf_history = inf_dat, ages = 1:80, maxage = 80)
