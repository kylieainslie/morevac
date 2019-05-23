### simulation

library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(doParallel)

#registerDoParallel(cores=2)
# load morevac package
#setwd("C:/Users/kainslie/Google Drive/morevac")
setwd("~/Google Drive/morevac")
devtools::load_all()

# follow a cohort from 2000 to 2019
sim <- 10
nindiv <- 5000
year_range <- 2000:2019
age_range <- 0:19
vaccov <- c(0, 0.25, 0.5, 0.75, 1)
#vaccov= 0.5
#start_year <- 1820

out0 <- array(NA,dim=c(200,80,sim))
outa <- array(NA,dim=c(200,80,sim))
outb <- array(NA,dim=c(200,80,sim))
life_inf0 <- matrix(c(rep(NA,sim*length(age_range))),nrow=sim)
life_infa <- life_inf0
life_infb <- life_inf0
ar_out0 <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
ar_outa <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
ar_outb <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))

#foreach (vc=1:length(vaccov)) %dopar% {
#  print(paste('Simulating for vaccination coverage', vaccov[vc]))
  # create progress bar
  pb <- txtProgressBar(min = 0, max = sim, style = 3)

  for (s in 1:sim){
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, s)

    # run model
    test0 <- multiannual2(n=nindiv, vac_coverage = 0, suscept_func_version = 2)
    testa <- multiannual2(n=nindiv, vac_coverage = vaccov, suscept_func_version = 2)
    testb <- multiannual2(n=nindiv, vac_coverage = vaccov,suscept_func_version = 2, biannual = TRUE)
    # attack rate by age
    out0[,,s] <- test0[[1]]$attack_rate_by_age
    dimnames(out0)[[1]] <- rownames(test0[[1]]$attack_rate_by_age)
    ar_out0[,s] <- diag(out0[rownames(out0) %in% year_range,(age_range+1),s])

    outa[,,s] <- testa[[1]]$attack_rate_by_age
    dimnames(outa)[[1]] <- rownames(testa[[1]]$attack_rate_by_age)
    ar_outa[,s] <- diag(outa[rownames(outa) %in% year_range,(age_range+1),s])

    outb[,,s] <- testb[[1]]$attack_rate_by_age
    dimnames(outb)[[1]] <- rownames(testb[[1]]$attack_rate_by_age)
    ar_outb[,s] <- diag(outb[rownames(outb) %in% year_range,(age_range+1),s])
    # lifetime infections
    tmp0 <- test0[[1]]$history$lifetime_infections[,,200]
    life_inf0[s,] <- apply(tmp0[which(!is.na(tmp0[,20]) & is.na(tmp0[,21])),age_range + 1],2,mean,na.rm = TRUE)
    tmpa <- testa[[1]]$history$lifetime_infections[,,200]
    life_infa[s,] <- apply(tmpa[which(!is.na(tmpa[,20]) & is.na(tmpa[,21])),age_range + 1],2,mean,na.rm = TRUE)
    tmpb <- testb[[1]]$history$lifetime_infections[,,200]
    life_infb[s,] <- apply(tmpb[which(!is.na(tmpb[,20]) & is.na(tmpb[,21])),age_range + 1],2,mean,na.rm = TRUE)
  }
  close(pb)

  cohort <- data.frame(Year = c(rep(year_range,3)),
                       Attack_Rate = c(apply(ar_out0,1,mean),
                                       apply(ar_outa,1,mean),
                                       apply(ar_outb,1,mean)),
                       Lower = c(apply(ar_out0,1,FUN = function(x) quantile(x, c(0.025))),
                                 apply(ar_outa,1,FUN = function(x) quantile(x, c(0.025))),
                                 apply(ar_outb,1,FUN = function(x) quantile(x, c(0.025)))),
                       Upper = c(apply(ar_out0,1,FUN = function(x) quantile(x, c(0.975))),
                                 apply(ar_outa,1,FUN = function(x) quantile(x, c(0.975))),
                                 apply(ar_outb,1,FUN = function(x) quantile(x, c(0.975)))),
                       Age = c(rep(age_range,3)),
                       Vac_Strategy = c(rep('No Vaccination',length(year_range)),
                                        rep('Annual',length(year_range)),
                                        rep('Biannual',length(year_range)))
  )

  p_cohort <-
     ggplot(data = cohort, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
     geom_line() +
     geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Vac_Strategy),alpha=0.2)+
     xlab('Year') +
     ylab('Attack Rate') +
     scale_y_continuous(limits = c(0,0.4), expand = c(0,0)) +
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

  #plot_attack_rates(dat = cohort, by = 'Vac_Strategy', c_bands = TRUE)

  pdf(file = paste0("figures/ar_by_strategy_mult",vaccov,".pdf"))
  plot(p_cohort)
  dev.off()
  #
  # lifetime infections
  life_inf_dat <- data.frame(Sim = c(rep(1:sim,3)),
                             Vac_Strategy = c(rep('No Vaccination',sim),
                                              rep('Annual',sim),
                                              rep('Biannual',sim)),
                             rbind(life_inf0,life_infa,life_infb)
  )
  names(life_inf_dat) <- c('Sim','Vac_Strategy',c(paste0("Age",age_range)))
  data_long <- gather(life_inf_dat, Age, Life_Inf, Age0:Age19, factor_key=TRUE)
  data_long$Age <- as.factor(str_remove(data_long$Age, 'Age'))

  data_long$Age = with(data_long, reorder(Age, Life_Inf, mean))
  # boxplot
  p1 <-
     ggplot(data_long, aes(x = Age, y = Life_Inf,fill = Vac_Strategy)) +
     geom_boxplot() +
     ylab('Number of Lifetime Infections') +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"),
           legend.position = c(.25, .95),
           legend.justification = c("right", "top"),
           legend.box.just = "right",
           legend.margin = margin(6, 6, 6, 6),
           legend.key = element_rect(fill = "white")
     )
  # plot_lifetime_infections(dat = data_long, by = 'Vac_Strategy')
  pdf(file = paste0("figures/life_inf_by_strategy_mult",vaccov,".pdf"))
  plot(p1)
  dev.off()

#}



