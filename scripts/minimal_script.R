# run code

# load required packages
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(cowplot)
# load morevac package
# setwd("C:/Users/kainslie/Google Drive/morevac")
  setwd("~/Google Drive/morevac")
  devtools::load_all()

# run multi-annual model
  out <- multiannual2()

# outputs
  p1 <- plot_attack_rates(dat = out[[1]]$attack_rate)
#  p_age <- plot_attack_rates(dat = out[[1]]$attack_rate_by_age, by = '')
