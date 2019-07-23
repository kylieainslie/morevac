# run code

# load required packages
  library(ggplot2)
  library(tidyr)
  library(reshape2)
  library(cowplot)
  library(stringr)

# load morevac package
#  setwd("C:/Users/kainslie/Documents/GitHub/morevac")
  setwd("~/Documents/morevac")
  devtools::load_all()

# run multi-annual model
  out <- multiannual2()

# outputs
  # plot attack rates
  p1 <- plot_attack_rates(dat = out$attack_rate)
  # plot lifetime infections
  inf_dat <- out$history[,,1]
  lifetime_inf <- get_lifetime_inf(inf_history = inf_dat, ages = 1:80, maxage = 80)

# plot drift
mydat <- out$drift
mydat$Cum_Drift <- cumsum(mydat$Drift)
mydat_thinned <- mydat[which(mydat$Vac_Update==1),]
p_drift <- ggplot(data = mydat, aes(x=Year, y=Cum_Drift)) +
           geom_line() +
           geom_point(data=mydat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red')
p_drift

# zoom in on vaccinated years
mydat_vac <- mydat[181:200,]
p_vac <- ggplot(data = mydat_vac, aes(x=Year, y=Cum_Drift)) +
         geom_line() +
         geom_point(data=mydat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red')
p_vac

# plot simulated data
setwd("/Volumes/kainslie/morevac_sims/data")

yearRange = 2000:2019
ageRange = 0:19

tags1 <- c("vs0vc50r09v1_w_drift","vs1vc50r09v1_w_drift","vs2vc50r09v1_w_drift")                                # filename tags
sim_out1 <- read_in_sim_data(tags1)                                               # read in sim results files
dat1 <- process_sim_output(sim_out1, year_range = yearRange, age_range = ageRange) # process sim data
pa1 <- plot_attack_rates(dat = dat1[[1]], by_vac = TRUE, c_bands = TRUE)          # plot attack rates
pl1 <- plot_lifetime_infections(dat = dat1[[2]], by_vac = TRUE, x=0.5)            # plot lifetime infections

