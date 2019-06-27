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
test <- multiannual2()
mydat <- test$drift
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

tags1 <- c("vs0vc0r0v1_w_drift","vs1vc0r0v1_w_drift","vs2vc0r0v1_w_drift")                                # filename tags
sim_out1 <- read_in_sim_data(tags1)                                               # read in sim results files
dat1 <- process_sim_output(sim_out1, year_range = yearRange, age_range = ageRange) # process sim data
pa1 <- plot_attack_rates(dat = dat1[[1]], by_vac = TRUE, c_bands = TRUE)          # plot attack rates
pl1 <- plot_lifetime_infections(dat = dat1[[2]], by_vac = TRUE, x=0.5)            # plot lifetime infections

tags2 <- c("vs0vc50r0v1_w_drift","vs1vc50r0v1_w_drift","vs2vc50r0v1_w_drift")                                # filename tags
sim_out2 <- read_in_sim_data(tags2)                                               # read in sim results files
dat2 <- process_sim_output(sim_out2, year_range = yearRange, age_range = ageRange) # process sim data
pa2 <- plot_attack_rates(dat = dat2[[1]], by_vac = TRUE, c_bands = TRUE)          # plot attack rates
pl2 <- plot_lifetime_infections(dat = dat2[[2]], by_vac = TRUE, x=0.5)            # plot lifetime infections

tags3 <- c("vs0vc50r0v2_w_drift","vs1vc50r0v2_w_drift","vs2vc50r0v2_w_drift")                                # filename tags
sim_out3 <- read_in_sim_data(tags3)                                               # read in sim results files
dat3 <- process_sim_output(sim_out3, year_range = yearRange, age_range = ageRange) # process sim data
pa3 <- plot_attack_rates(dat = dat3[[1]], by_vac = TRUE, c_bands = TRUE)          # plot attack rates
pl3 <- plot_lifetime_infections(dat = dat3[[2]], by_vac = TRUE, x=0.5)            # plot lifetime infections


