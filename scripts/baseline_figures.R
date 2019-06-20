### baseline figures (attack rate and lifetime infections)
  # vc = 0, 0.25, 0.5, 0.75, 1
  # rho = 0
  # version = 1

setwd("Q:/morevac_sims/data")

tags1 <- c("vs0vc0r0v2","vs1vc0r0v2","vs2vc0r0v2")                                # filename tags
sim_out1 <- read_in_sim_data(tags1)                                               # read in sim results files
dat1 <- process_sim_output(sim_out1, year_range = yearRange, age_range = ageRange) # process sim data
pa1 <- plot_attack_rates(dat = dat1[[1]], by_vac = TRUE, c_bands = TRUE)          # plot attack rates
pl1 <- plot_lifetime_infections(dat = dat1[[2]], by_vac = TRUE, x=0.5)            # plot lifetime infections

tags2 <- c("vs0vc0r0v2","vs1vc25r0v2","vs2vc25r0v2")
sim_out2 <- read_in_sim_data(tags2)
dat2 <- process_sim_output(sim_out2, year_range = yearRange, age_range = ageRange)
pa2 <- plot_attack_rates(dat = dat2[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl2 <- plot_lifetime_infections(dat = dat2[[2]], by_vac = TRUE, no_legend = TRUE)

tags3 <- c("vs0vc0r0v2","vs1vc50r0v2","vs2vc50r0v2")
sim_out3 <- read_in_sim_data(tags3)
dat3 <- process_sim_output(sim_out3, year_range = yearRange, age_range = ageRange)
pa3 <- plot_attack_rates(dat = dat3[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl3 <- plot_lifetime_infections(dat = dat3[[2]], by_vac = TRUE, no_legend = TRUE)

tags4 <- c("vs0vc0r0v2","vs1vc75r0v2","vs2vc75r0v2")
sim_out4 <- read_in_sim_data(tags4)
dat4 <- process_sim_output(sim_out4, year_range = yearRange, age_range = ageRange)
pa4 <- plot_attack_rates(dat = dat4[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl4 <- plot_lifetime_infections(dat = dat4[[2]], by_vac = TRUE, no_legend = TRUE)

tags5 <- c("vs0vc0r0v2","vs1vc1r0v2","vs2vc1r0v2")
sim_out5 <- read_in_sim_data(tags5)
dat5 <- process_sim_output(sim_out5, year_range = yearRange, age_range = ageRange)
pa5 <- plot_attack_rates(dat = dat5[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl5 <- plot_lifetime_infections(dat = dat5[[2]], by_vac = TRUE, no_legend = TRUE)

# plots
theme_set(theme_cowplot(font_size=10)) # reduce default font size
attack_rate_plot <- plot_grid(pa1, pa2, pa3, pa4, pa5,labels = "AUTO", ncol = 2,
                              align = 'v', axis = 'l') # aligning vertically along the left axis
lifetime_inf_plot <- plot_grid(pl1, pl2, pl3, pl4, pl5, labels = "AUTO", ncol = 2,
                               align = 'v', axis = 'l') # aligning vertically along the left axis


setwd("Q:/morevac_sims/figures")

pdf(file = "baseline_ar_plot_v2.pdf")
plot(attack_rate_plot)
dev.off()

pdf(file = "baseline_li_plot_v2.pdf")
plot(lifetime_inf_plot)
dev.off()
