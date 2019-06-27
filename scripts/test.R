### testing script
# check drift and vac update
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

# check susceptibility calculation
inf_hist <- test$history[,,1]
vac_hist <- test$history[,,2]
 suscept <- test$history[,,3]

# subset cohort

