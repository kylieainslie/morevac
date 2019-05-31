# run code

# load required packages
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(cowplot)
  library(stringr)

# load morevac package
# setwd("C:/Users/kainslie/Google Drive/morevac")
  setwd("~/Documents/morevac")
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
  p1 <- plot_attack_rates(dat = out$attack_rate)
#  p_age <- plot_attack_rates(dat = out[[1]]$attack_rate_by_age, by = '')
