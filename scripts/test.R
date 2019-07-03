### testing script ###
test <- multiannual2(wane = 0.84, suscept_func_version = 2)
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

i <- 1
person <- data.frame(inf_hist = oc_inf_hist[i,],
                     x = oc_x[i,],
                     vac_hist = oc_vac_hist[i,],
                     v = oc_v[i,],
                     suscept  = oc_suscept[i,],
                     drift = mydat_orig$Drift,
                     vac_dist = mydat_orig$Vac_Distance,
                     update = mydat_orig$Vac_Update)
person
# birth cohort
birth_cohort <- which(rownames(vac_hist)==19)
bc_vac_hist <- vac_hist[birth_cohort,1:20]
bc_inf_hist <- inf_hist[birth_cohort,1:20]
 bc_suscept <- suscept[birth_cohort,1:20]
       bc_x <- x[birth_cohort,1:20]
       bc_v <- v[birth_cohort,1:20]

i <- 1
person <- data.frame(inf_hist = bc_inf_hist[i,],
                            x = bc_x[i,],
                     vac_hist = bc_vac_hist[i,],
                           v = bc_v[i,],
                     suscept  = bc_suscept[i,],
                     drift = mydat_vac$Drift,
                     vac_dist = mydat_vac$Vac_Distance,
                     update = mydat_vac$Vac_Update)
person
