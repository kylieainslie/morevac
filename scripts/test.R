### test run and plots ###

###  run simulation
# no drift
annual1_nodrift0 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 0, mygamma = 0)
annual1_nodrift25 <- run_annual1(n = 10000, years = 17, vac_coverage = 0.25, mydelta = 0, mygamma = 0)
annual1_nodrift50 <- run_annual1(n = 10000, years = 17, vac_coverage = 0.5, mydelta = 0, mygamma = 0)
annual1_nodrift75 <- run_annual1(n = 10000, years = 17, vac_coverage = 0.75, mydelta = 0, mygamma = 0)
annual1_nodrift100 <- run_annual1(n = 10000, years = 17, vac_coverage = 1, mydelta = 0, mygamma = 0)

### create plot
nodrift_ar <- data.frame(year = c(rep(1:17,5)),
                         attack_rate = c(annual1_nodrift0[[1]]$attack_rate,
                                         annual1_nodrift25[[1]]$attack_rate,
                                         annual1_nodrift50[[1]]$attack_rate,
                                         annual1_nodrift75[[1]]$attack_rate,
                                         annual1_nodrift100[[1]]$attack_rate),
                         vac_coverage = c(rep(0,17),rep(0.25,17),rep(0.50,17),
                                          rep(0.75,17),rep(1,17)))

p_nodrift <- ggplot(data = nodrift_ar, aes(x = year, y = attack_rate, 
                                           colour = as.factor(vac_coverage))) +       
  geom_line()

pdf(file = "figures/annual_nodrift.pdf")
p_nodrift
dev.off()

### drift

annual1_drift0 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 0)
annual1_drift2 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 0.2)
annual1_drift4 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 0.4)
annual1_drift6 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 0.6)
annual1_drift8 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 0.8)
annual1_drift10 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, mydelta = 1)

drift_ar <- data.frame(year = c(rep(1:17,6)),
                       attack_rate = c(annual1_drift0[[1]]$attack_rate,
                                       annual1_drift2[[1]]$attack_rate,
                                       annual1_drift4[[1]]$attack_rate,
                                       annual1_drift6[[1]]$attack_rate,
                                       annual1_drift8[[1]]$attack_rate,
                                       annual1_drift10[[1]]$attack_rate),
                       drift = c(rep(0,17),rep(0.2,17),rep(0.4,17),
                                 rep(0.6,17),rep(0.8,17),rep(1.0,17)))
p_drift <- ggplot(data = drift_ar, aes(x = year, y = attack_rate, colour = as.factor(drift))) +       
  geom_line()
pdf(file = "figures/annual_drift.pdf")
plot(p_drift)
dev.off()

#annual1_drift[[2]]

### biannual plots ##############

###  run simulation

# no drift
biannual1_nodrift0 <- run_annual1(n = 10000, years = 17, vac_coverage = 0, 
                                  mydelta = 0, mygamma = 0, biannual = TRUE)
biannual1_nodrift25 <- run_annual1(n = 10000, years = 17, vac_coverage = 0.25, 
                                   mydelta = 0, mygamma = 0, biannual = TRUE)
biannual1_nodrift50 <- run_annual1(n = 10000, years = 17, vac_coverage = 0.5, 
                                   mydelta = 0, mygamma = 0, biannual = TRUE)
biannual1_nodrift75 <- run_annual1(n = 10000, years = 17, vac_coverage = 0.75,
                                   mydelta = 0, mygamma = 0, biannual = TRUE)
biannual1_nodrift100 <- run_annual1(n = 10000, years = 17, vac_coverage = 1, 
                                    mydelta = 0, mygamma = 0, biannual = TRUE)

### create plot
bi_nodrift_ar <- data.frame(year = c(rep(1:17,5)),
                            attack_rate = c(biannual1_nodrift0[[1]]$attack_rate,
                                            biannual1_nodrift25[[1]]$attack_rate,
                                            biannual1_nodrift50[[1]]$attack_rate,
                                            biannual1_nodrift75[[1]]$attack_rate,
                                            biannual1_nodrift100[[1]]$attack_rate),
                            vac_coverage = c(rep(0,17),rep(0.25,17),rep(0.50,17),
                                             rep(0.75,17),rep(1,17)))

p_bi_nodrift <- ggplot(data = bi_nodrift_ar, aes(x = year, y = attack_rate, 
                                                 colour = as.factor(vac_coverage))) +       
  geom_line()

pdf(file = "figures/biannual_nodrift.pdf")
p_bi_nodrift
dev.off()

### drift (no vaccination)

biannual1_drift0 <- run_annual1(n = 10000, years = 17, vac_coverage = 0,
                                mydelta = 0, biannual = TRUE)
biannual1_drift2 <- run_annual1(n = 10000, years = 17, vac_coverage = 0,
                                mydelta = 0.2, biannual = TRUE)
biannual1_drift4 <- run_annual1(n = 10000, years = 17, vac_coverage = 0,
                                mydelta = 0.4, biannual = TRUE)
biannual1_drift6 <- run_annual1(n = 10000, years = 17, vac_coverage = 0,
                                mydelta = 0.6, biannual = TRUE)
biannual1_drift8 <- run_annual1(n = 10000, years = 17, vac_coverage = 0,
                                mydelta = 0.8, biannual = TRUE)
biannual1_drift10 <- run_annual1(n = 10000, years = 17, vac_coverage = 0,
                                 mydelta = 1, biannual = TRUE)

bi_drift_ar <- data.frame(year = c(rep(1:17,6)),
                          attack_rate = c(biannual1_drift0[[1]]$attack_rate,
                                          biannual1_drift2[[1]]$attack_rate,
                                          biannual1_drift4[[1]]$attack_rate,
                                          biannual1_drift6[[1]]$attack_rate,
                                          biannual1_drift8[[1]]$attack_rate,
                                          biannual1_drift10[[1]]$attack_rate),
                          drift = c(rep(0,17),rep(0.2,17),rep(0.4,17),
                                    rep(0.6,17),rep(0.8,17),rep(1.0,17)))
p_bi_drift <- ggplot(data = bi_drift_ar, aes(x = year, y = attack_rate,
                                             colour = as.factor(drift))) +       
  geom_line()
pdf(file = "figures/biannual_drift.pdf")
plot(p_bi_drift)
dev.off()