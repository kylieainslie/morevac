
library(ggplot2)
library(cowplot)

test <- multiannual2(n=5000)
# plot attack rate by age
ar_dat <- test[[1]]$attack_rate_by_age
age_dat <- cbind(as.numeric(rownames(ar_dat)),apply(ar_dat[,1:10], 1, mean),apply(ar_dat[,11:20], 1, mean),
                 apply(ar_dat[,21:30], 1, mean),apply(ar_dat[,31:40], 1, mean),apply(ar_dat[,41:50], 1, mean),
                 apply(ar_dat[,51:60], 1, mean),apply(ar_dat[,61:70], 1, mean),apply(ar_dat[,71:80], 1, mean))
colnames(age_dat) <- c('Year','0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79')

keycol <- "Age_Group"
valuecol <- "Attack_Rate"
gathercols <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79')
age_dat1 <- gather_(as.data.frame(age_dat), keycol, valuecol, gathercols)


p_age <- plot_attack_rates(dat=age_dat1,by_age = TRUE)

# kids only

kids_only <- data.frame(as.numeric(rownames(ar_dat)),ar_dat[,1:10])
colnames(kids_only) <- c('Year','0','1','2','3','4','5','6','7','8','9')

keycol <- "Age_Group"
valuecol <- "Attack_Rate"
gathercols <- c('0','1','2','3','4','5','6','7','8','9')
kids_only1 <- gather_(kids_only, keycol, valuecol, gathercols)

p_kids <- plot_attack_rates(dat=kids_only1[kids_only1$Year %in% (2000:2019),],by_age = TRUE)

# follow a cohort from 2000 to 2019
sim <- 100
nindiv <- 5000
year_range <- 2000:2019
age_range <- 0:19
vaccov <- c(0, 0.25, 0.5, 0.75, 1)
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

for (vc in vaccov){
  print(paste('Simulating for vaccination coverage', vc))
# create progress bar
pb <- txtProgressBar(min = 0, max = sim, style = 3)

for (s in 1:sim){
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb, s)

  # run model
  test0 <- multiannual2(n=nindiv, vac_coverage = 0)
  testa <- multiannual2(n=nindiv, vac_coverage = vc)
  testb <- multiannual2(n=nindiv, vac_coverage = vc, biannual = TRUE)
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

p_cohort <- ggplot(data = cohort, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
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
pdf(file = paste0("figures/ar_by_strategy_mult",vc,".pdf"))
plot(p_cohort)
dev.off()

# lifetime infections
library(stringr)
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
p1 <- ggplot(data_long, aes(x = Age, y = Life_Inf,fill = Vac_Strategy)) +
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

pdf(file = paste0("figures/life_inf_by_strategy_mult",vc,".pdf"))
plot(p1)
dev.off()

#theme_set(theme_cowplot(font_size=10)) # reduce default font size
#omg <- plot_grid(p_cohort, p1, labels = "AUTO", ncol = 2,
#                 align = 'v', axis = 'l') # aligning vertically along the left axis
#pdf(file = paste0("figures/combined_plot_",vc,".pdf"))
#plot(omg)
#dev.off()

}

