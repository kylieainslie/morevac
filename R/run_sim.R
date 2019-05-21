### Simulation2 - using multiannual2 (delta_x ne delta_v)

library(ggplot2)
library(cowplot)
library(tidyr)
library(reshape2)
### drift and vaccination
gam = 0.4
dx = 0.2
dv = 0.2
years=17
sim = 20

run_sim <- function(sim=100, nindiv=1000, years=17, vc= 0.5, gam=0.4, dx=0.2, dv=0.2, si=FALSE){
  # initialize output matrices
  outputa <- data.frame(matrix(c(rep(NA,sim*(years+2))),ncol=years+2))
  outputa[,2] <- c(rep('annual',sim))
  names(outputa) <- c('sim','vac_strategy',paste0('AY',1:17))

  outputb <- data.frame(matrix(c(rep(NA,sim*(years+2))),ncol=years+2))
  colnames(outputb) <- c('sim','vac_strategy',paste0('AY',1:17))
  outputb[,2] <- c(rep('biannual',sim))

  # create progress bar
  pb <- txtProgressBar(min = 0, max = sim, style = 3)
  # start simulation
  for (s in 1:sim){
    #print(s)
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, s)
    # annual
    outputa[s,1] <- s
    annual <- multiannual2(n = nindiv, years = years, vac_coverage = vc, delta_x = dx, delta_v = dv,
                           mygamma = gam, same_indiv = si)
    outputa[s,3:dim(outputa)[2]] <- as.vector(annual[[1]]$attack_rate)
    # biannual
    outputb[s,1] <- s
    biannual <- multiannual2(n = nindiv, years = years, vac_coverage = vc, delta_x = dx, delta_v = dv,
                             mygamma = gam, biannual = TRUE, same_indiv = si)
    outputb[s,3:dim(outputb)[2]] <- as.vector(biannual[[1]]$attack_rate)
  
  }
  close(pb)
  
  # plot AR by vac strategy
  ar_plot <- plot_ar_by_vac_strategy(outputa,outputb)
  return(list(out_annual=outputa,out_biannual=outputb,ar_plot))
}

plot_ar_by_vac_strategy <- function(outputa, outputb){
  
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

  return(p1)
}

#pdf(file = "figures/ar_by_strategy_si_vc25.pdf")
#plot(p1)
#dev.off()