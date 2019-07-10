### testing script ###
test <- multiannual2(wane = 1, suscept_func_version = 2, vac_strategy = 2)
p_test <- plot_attack_rates(dat = test$attack_rate)
p_test
# check drift and vac update
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
         ylab('Cummulative Drift') +
         geom_point(data=mydat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red')
p_vac

# check susceptibility calculation
inf_hist <- test$history[,,1]
vac_hist <- test$history[,,2]
 suscept <- test$history[,,3]
       x <- test$history[,,4]
       v <- test$history[,,5]

# original cohort
mydat_orig <- mydat[161:200,]
orig_cohort <- which(rownames(vac_hist)==39)
oc_vac_hist <- vac_hist[orig_cohort,1:40]
oc_inf_hist <- inf_hist[orig_cohort,1:40]
oc_suscept <- suscept[orig_cohort,1:40]
oc_x <- x[orig_cohort,1:40]
oc_v <- v[orig_cohort,1:40]

# i <- 1
# person <- data.frame(inf_hist = oc_inf_hist[i,],
#                      x = oc_x[i,],
#                      vac_hist = oc_vac_hist[i,],
#                      v = oc_v[i,],
#                      suscept  = oc_suscept[i,],
#                      drift = mydat_orig$Drift,
#                      vac_dist = mydat_orig$Vac_Distance,
#                      update = mydat_orig$Vac_Update)
# person

# birth cohort
birth_cohort <- which(rownames(vac_hist)==19)
bc_vac_hist <- vac_hist[birth_cohort,1:20]
bc_inf_hist <- inf_hist[birth_cohort,1:20]
 bc_suscept <- suscept[birth_cohort,1:20]
       bc_x <- x[birth_cohort,1:20]
       bc_v <- v[birth_cohort,1:20]

i <- 3
person <- data.frame(inf_hist = bc_inf_hist[i,],
                            x = bc_x[i,],
                     vac_hist = bc_vac_hist[i,],
                           v = bc_v[i,],
                     suscept  = bc_suscept[i,],
                     drift = round(mydat_vac$Drift,4),
                     vac_dist = mydat_vac$Vac_Distance,
                     update = mydat_vac$Vac_Update)
person$Year <- 2000:2019
person$no_vac_suscept <- c(rep(0,20))
for (i in 1:20){
  if (person$x[i]>=999){
          person$no_vac_suscept[i] <- 1
  } else if (person$x[i] == 0){
          person$no_vac_suscept[i] <- 0
  } else {
          person$no_vac_suscept[i] <- min(1,person$no_vac_suscept[i-1] + person$drift[i])
  }
}
vax <- person$Year[which(person$vac_hist == 1)]
infections <- person$Year[which(person$inf_hist == 1)]

# plot susceptibility
p_suscept <- ggplot(data = person, aes(x = Year, y = suscept)) +
             geom_line() +
             geom_line(aes(y = no_vac_suscept),colour = 'blue') +
             xlab('Year') +
             ylab('Susceptibility') +
             scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

p_suscept <- p_suscept +
             geom_vline(xintercept=vax, linetype="dashed", colour = 'black') +
             geom_vline(xintercept=infections, colour = 'red')

library(cowplot)
#theme_set(theme_cowplot(font_size=10)) # reduce default font size
p <- plot_grid(p_vac, p_suscept, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
p
