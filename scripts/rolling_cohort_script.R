# rolling cohort script

# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(foreach)
library(doParallel)
library(dplyr)
library(plyr)
library(lhs)
library(boot)
library(data.table)
# load morevac package
setwd("~/Documents/morevac")
devtools::load_all()
#######################################
# specify save directory
setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data")
### simulation
## create latin hypercube of parameter values to simulate over
set.seed(1234)
#set.seed(5678)
mylhc <- randomLHS(1000, 6)
colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
mylhc[, "Epsilon"] <- qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.1)
mylhc[, "Vac_Cov"] <- qunif(mylhc[,"Vac_Cov"], min = 0, max = 0.5) # range from 2018-2019 PHE estimates of VC in school-aged children: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/806289/Childhood_flu_annual_report_2018_19_FINAL_.pdf
mylhc[, "Take"] <- qunif(mylhc[,"Take"], min = 0.5, max = 1)
# write hyper cube parameters to file
write.csv(mylhc,file = "latin_hyper_cube_parameters.csv")
# run simulations
# parallelize
bins <- list(seq(1,100),seq(101,200),seq(201,300),seq(301,400),seq(401,500),
             seq(501,600),seq(601,700),seq(701,800),seq(801,900),seq(901,1000))
# make cluster
# ncl <- detectCores()
# cl <- makeCluster(ncl)
# registerDoParallel(cl)

for (b in 1:10){
#foreach(b=1:10, .packages = c('morevac','Rcpp')) %dopar% {

loop_length <- length(bins[[b]])
for (i in 1:loop_length){
cat("\n Simulation ",i," of",loop_length,"\n")
# parameters
n_sim = 10
nindiv <- 30000
row_lhc <- bins[[b]][i]
max_age = 80
myyears <- 1820:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 16
vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- mylhc[row_lhc,"Vac_Cov"]
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- mylhc[row_lhc,"Vac_Cov"]
# output note to user
cat("\n No vaccination simulation running... \n")
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_Vac, vac_strategy = 0,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Annual vaccination simulation running... \n")
sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Annual, vac_strategy = 1,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Every other year vaccination simulation running... \n")
sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Biennial, vac_strategy = 2,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])

# extract cohorts from each sim and combine raw inf and vac histories for every simulation
sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = myyears, nsim = n_sim)
sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = myyears, nsim = n_sim)
sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = myyears, nsim = n_sim)

# combine into one data.table
inf_histories <- rbindlist(list(No_Vac = sim0_results$inf_history, Annual = sim1_results$inf_history, Biennial = sim2_results$inf_history), idcol = 'Vac_Strategy')
vac_histories <- rbindlist(list(No_Vac = sim0_results$vac_history, Annual = sim1_results$vac_history, Biennial = sim2_results$vac_history), idcol = 'Vac_Strategy')

# add parameter value index column
inf_histories$Param_Index <- row_lhc
vac_histories$Param_Index <- row_lhc

# write raw output to file
file <- paste0("sim_",row_lhc)
try(data.table::fwrite(inf_histories, file = paste0("infection_histories/",file,"_inf_hist.csv"), col.names = TRUE,
                       row.names = FALSE, sep = ","))
try(data.table::fwrite(vac_histories, file = paste0("vaccination_histories/",file,"_vac_hist.csv"), col.names = TRUE,
                       row.names = FALSE, sep = ","))

} # end b bin loop
} # b loop

# stop cluster
# stopCluster(cl)

#######################################
# read in results (rather than re-run simulations)
setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/infection_histories")
files_inf <- list.files(pattern="*.csv")
dt_inf = do.call(rbind, lapply(files, fread))

setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/vaccination_histories")
files_vac <- list.files(pattern="*.csv")
dt_vac = do.call(rbind, lapply(files_vac, fread))

setwd("~/Dropbox/Kylie/Projects/Morevac/data")
param_values <- read.csv("parameter_values.csv", header = TRUE)
names(param_values)[1] <- "Param_Index"
### summarise raw data
dt_inf1 <- dt_inf %>% mutate(Num_Infs = rowSums(select(.,Age0:Age18)))
dt_vac1 <- dt_vac %>% mutate(Num_Vacs = rowSums(select(.,Age0:Age18)))

banana <- cbind(dt_inf1[,c("Vac_Strategy", "Sim", "Cohort", "ID", "Param_Index", "Num_Infs")],dt_vac1[,c("Num_Vacs")])
banana_boat <- banana %>% group_by(Vac_Strategy, Param_Index, Num_Vacs) %>% summarise(Mean_Infs = mean(Num_Infs))

# investigate fully vaccinated individuals
vac_max <- c(max(dt_vac1[dt_vac1$Vac_Strategy == "Annual"]$Num_Vacs), max(dt_vac1[dt_vac1$Vac_Strategy == "Biennial"]$Num_Vacs))
indivs <- dt_vac1$ID[dt_vac$Vac_Strategy == "Annual" & dt_vac1$Num_Vacs == vac_max[1] |
                     dt_vac$Vac_Strategy == "Biennial" & dt_vac1$Num_Vacs == vac_max[2]]
dt_inf2 <- dt_inf1[dt_inf1$ID %in% indivs & dt_inf1$Vac_Strategy != "No_Vac",]
dt_inf_avg <- dt_inf2 %>% group_by(Vac_Strategy, Param_Index) %>% summarise_at(.vars = paste0("Age",0:18),.funs = c(Mean="mean"))
# convert to long format
dt_inf_avg1 <- dt_inf_avg %>% gather(Age, Mean, Age0_Mean:Age18_Mean)        # long format
dt_inf_avg1$Age <- as.numeric(substr(dt_inf_avg1$Age,4,4))                   # cut string to only age #
dt_inf_avg2 <- left_join(dt_inf_avg1, param_values, by = c("Param_Index"))   # add parameter value columns




















