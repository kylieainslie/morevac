### simulation

library(ggplot2)
library(cowplot)
library(tidyr)
library(reshape2)
### drift and vaccination
gam = 0.4
delt = 0.2
years=17
sim = 100

outputa <- data.frame(matrix(c(rep(NA,sim*(years+2))),ncol=years+2))
outputa[,2] <- c(rep('annual',sim))
names(outputa) <- c('sim','vac_strategy',paste0('AY',1:17))

outputb <- data.frame(matrix(c(rep(NA,sim*(years+2))),ncol=years+2))
colnames(outputb) <- c('sim','vac_strategy',paste0('AY',1:17))
outputb[,2] <- c(rep('biannual',sim))

for (s in 1:sim){
  print(s)
# annual
outputa[s,1] <- s
annual <- run_annual1(n = 10000, years = 17, vac_coverage = 1, mydelta = delt, 
                             mygamma = gam, same_indiv = FALSE)
outputa[s,3:dim(outputa)[2]] <- as.vector(annual[[1]]$attack_rate)
# biannual
outputb[s,1] <- s
biannual <- run_annual1(n = 10000, years = 17, vac_coverage = 1,mydelta = delt, 
                               mygamma = gam, biannual = TRUE, same_indiv = FALSE)
outputb[s,3:dim(outputb)[2]] <- as.vector(biannual[[1]]$attack_rate)

}

quanta <- apply(outputa[,3:19],2, FUN = function(x) quantile(x, c(0.025,0.5,0.975)))
quanta <-melt(quanta)
names(quanta) <- c('quantile','year','value')
quanta$year <- as.numeric(gsub('AY','',quanta$year))
quanta$strategy <- as.factor(c(rep('annual',dim(quanta)[1])))


quantb <- apply(outputb[,3:19],2, FUN = function(x) quantile(x, c(0.025,0.5,0.975)))
quantb <-melt(quantb)
names(quantb) <- c('quantile','year','value')
quantb$year <- as.numeric(gsub('AY','',quantb$year))
quantb$strategy <- as.factor(c(rep('biannual',dim(quantb)[1])))

quantiles <- rbind(quanta,quantb)
quantiles <- quantiles[order(quantiles$year),]
quantiles1 <- spread(quantiles,quantile,value)
names(quantiles1)[3:5] <- c('lower','median','upper')
#out1 <- gather(out,year,attack_rate,AY1:AY17,factor_key=TRUE)
#out1[,'year'] <- as.numeric(gsub('AY','',out1[,'year']))
#out1[,'attack_rate'] <- as.numeric[,'attack_rate']
#out1[,'vac_strategy'] <- as.factor(out1[,'vac_strategy'])


p1 <- ggplot(data = quantiles1,aes(x = year, y = median, colour = strategy, group=strategy)) +
      geom_line()+
      geom_ribbon(aes(x=year,ymin=lower,ymax=upper,linetype=NA,fill=strategy),alpha=0.2)+
      xlab('Year') +
      ylab('Attack Rate') +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            legend.position = c(.95, .95),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),
            legend.key = element_rect(fill = "white")
      )

p1

pdf(file = "figures/ar_by_strategy_si_vc25.pdf")
plot(p1)
dev.off()



