# data
drift_dat <- data.frame(Year = 2000:2019,
                        Drift = c(0.11977404, 0.63730862, 0.43519019, 0.07572346, 0.06226929, 0.13514621, 0.01726972, 0.12709003, 0.04206521, 0.14560452,
                                  0.23989795, 0.12613324, 0.09959883, 0.05037801, 0.09989968, 0.84164972, 0.19035078, 0.13616622, 0.09772654, 0.34426955),
                        Vac_Update = c(0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1))
# add shade_group
drift_dat$Shade_Group <- c(rep(1,dim(drift_dat)[1]))

for (i in 2:dim(drift_dat)[1]){
  if (drift_dat$Vac_Update[i]==1){drift_dat$Shade_Group[i] <- drift_dat$Shade_Group[i-1] + 1
  } else {drift_dat$Shade_Group[i] <- drift_dat$Shade_Group[i-1]}
}

drift_dat$Shade_Group <- (drift_dat$Shade_Group)
#calculate cumulative drift
drift_dat$Cum_Drift <- cumsum(drift_dat$Drift)
drift_dat$Year  <-(drift_dat$Year)
# determine years when a vaccine update occurred
drift_dat_thinned <- drift_dat[which(drift_dat$Vac_Update==1),]
drift_dat_thinned$prev <-c(min(drift_dat$Year),drift_dat_thinned$Year[-nrow(drift_dat_thinned)])
# plot
ymx <- 4
p_drift <- ggplot(data = drift_dat, mapping=aes( x=Year,y = Cum_Drift))+
 geom_rect(data=drift_dat_thinned,
           aes(xmin=prev,ymin=0,ymax=ymx,xmax=Year,fill=factor(Shade_Group),
               col=factor(Shade_Group)),alpha=0.2) +
 # color shading under cumulative drift by shade group
 geom_ribbon(aes(ymin=Cum_Drift,ymax=ymx),fill="white" ,alpha=1) +
  ylab('Cumulative Antigenic Drift') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
geom_point(data = drift_dat_thinned,
           aes(x=Year,y=Cum_Drift, col = factor(Shade_Group))) +
geom_line()
p_drift

