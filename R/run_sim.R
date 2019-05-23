### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Run simulations of multi-annual model of infection and vaccination
#'
#' This function initializes the population before running the model.
#' @param sim number of simulations to run
#' @param nindiv number of individuals to be simulated
#' @param year_range range of years to follow cohort
#' @param age_range range of ages to follow cohort
#' @param vaccov vaccination coverage, should be between 0 and 1 (inclusive)
#' @param filename prefix of outputted figures
#' @param version which vaccination version to use: 1 = either-or, 2 = multiplicative
#' @return two plots 1) plot of annual attack rates for the cohort and 2) a plot of number of lifetime infections by age
#' @keywords morevac
#' @export

run_sim <- function(sim = 100, nindiv = 1000, year_range, age_range, vaccov = 0.5,filename = "test", version = 1){
### create empty arrays for storing information about each simulation
  out0 <- array(NA,dim=c(200,80,sim))
  outa <- array(NA,dim=c(200,80,sim))
  outb <- array(NA,dim=c(200,80,sim))

  life_inf0 <- matrix(c(rep(NA,sim*length(age_range))),nrow=sim)
  life_infa <- life_inf0
  life_infb <- life_inf0

  ar_out0 <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
  ar_outa <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))
  ar_outb <- matrix(c(rep(NA,sim*length(year_range))),nrow=length(year_range))

### create progress bar
  pb <- txtProgressBar(min = 0, max = sim, style = 3)
### start simulations
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

### output
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

  # p_cohort <-
  #    ggplot(data = cohort, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
  #    geom_line() +
  #    geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Vac_Strategy),alpha=0.2)+
  #    xlab('Year') +
  #    ylab('Attack Rate') +
  #    scale_y_continuous(limits = c(0,0.4), expand = c(0,0)) +
  #    theme(panel.grid.major = element_blank(),
  #          panel.grid.minor = element_blank(),
  #          panel.background = element_blank(),
  #          axis.line = element_line(colour = "black"),
  #          legend.position = c(.95, .95),
  #          legend.justification = c("right", "top"),
  #          legend.box.just = "right",
  #          legend.margin = margin(6, 6, 6, 6),
  #          legend.key = element_rect(fill = "white")
  #    )

  p_cohort <- plot_attack_rates(dat = cohort, by_vac = TRUE, c_bands = TRUE)

  pdf(file = paste0(filename,"_ar.pdf"))
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
  # p1 <-
  #    ggplot(data_long, aes(x = Age, y = Life_Inf,fill = Vac_Strategy)) +
  #    geom_boxplot() +
  #    ylab('Number of Lifetime Infections') +
  #    theme(panel.grid.major = element_blank(),
  #          panel.grid.minor = element_blank(),
  #          panel.background = element_blank(),
  #          axis.line = element_line(colour = "black"),
  #          legend.position = c(.25, .95),
  #          legend.justification = c("right", "top"),
  #          legend.box.just = "right",
  #          legend.margin = margin(6, 6, 6, 6),
  #          legend.key = element_rect(fill = "white")
  #    )

  p1 <- plot_lifetime_infections(dat = data_long, by_vac = TRUE)

  pdf(file = paste0(filename,"_life_inf.pdf"))
  plot(p1)
  dev.off()

}



