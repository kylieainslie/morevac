
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

### plot simulation results
tags1 <- c("vs0vc50r09v1w84","vs1vc50r09v1w84","vs2vc50r09v1w84")
sim_out1 <- list(no_vac = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags1[1],".csv"),header = TRUE)[,-1],
                               lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags1[1],".csv"),header = TRUE)[,-1]),
                 annual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags1[2],".csv"),header = TRUE)[,-1],
                               lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags1[2],".csv"),header = TRUE)[,-1]),
                 biannual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags1[3],".csv"),header = TRUE)[,-1],
                                 lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags1[3],".csv"),header = TRUE)[,-1])
)
# process data to be plotted
yearRange <- 2000:2019
ageRange <- 0:19
y_max <- 0.25
dat1 <- process_sim_output(sim_out1, year_range = yearRange, age_range = ageRange)
# plot attack rates
pa1 <- plot_attack_rates(dat = dat1[[1]], by_vac = TRUE, c_bands = TRUE)
# plot lifetime infections
pl1 <- plot_lifetime_infections(dat = dat1[[2]], by_vac = TRUE, x=0.5)

# plot attack rates for different values of waning
path <- 'Q:/morevac_sims/data/'
wane_val <- c('0','25','50','84','100')
for (i in 1:length(wane_val)){
  tags <- paste0(c("vs0vc50r09v1w","vs1vc50r09v1w","vs2vc50r09v1w"),c(rep(wane_val[i],3)))
  sim_out <- list(no_vac = list(attack_rate = read.csv(file = paste0(path,"attack_rates/attack_rate_data_",tags[1],".csv"),header = TRUE)[,-1],
                                lifetime_infections = read.csv(file = paste0(path,"lifetime_infections/lifetime_inf_data_",tags[1],".csv"),header = TRUE)[,-1]),
                  annual = list(attack_rate = read.csv(file = paste0(path,"attack_rates/attack_rate_data_",tags[2],".csv"),header = TRUE)[,-1],
                                lifetime_infections = read.csv(file = paste0(path,"lifetime_infections/lifetime_inf_data_",tags[2],".csv"),header = TRUE)[,-1]),
                  biannual = list(attack_rate = read.csv(file = paste0(path,"attack_rates/attack_rate_data_",tags[3],".csv"),header = TRUE)[,-1],
                                  lifetime_infections = read.csv(file = paste0(path,"lifetime_infections/lifetime_inf_data_",tags[3],".csv"),header = TRUE)[,-1]))
  dat <- process_sim_output(sim_out, year_range = yearRange, age_range = ageRange)
  dat[[1]]$wane <- c(rep(as.numeric(wane_val[i]),dim(dat[[1]])[1]))
  assign(paste0('dat',i),dat)
}

dat_all <- rbind(dat1[[1]],dat2[[1]],dat3[[1]],dat4[[1]],dat5[[1]])
dat_all$wane <- as.factor(dat_all$wane)
annual <- dat_all[dat_all$Vac_Strategy == 'Annual',]
biannual <- dat_all[dat_all$Vac_Strategy == 'Biannual',]

p_wane <- ggplot(data = biannual, aes(x = Year, y = Attack_Rate, colour= wane)) +
  geom_line() +
  xlab('Year') +
  ylab('Attack Rate') +
  scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "white"))

p_wane


