### Latin Hypercube simulations script ###
# last modified: 07/10/2020

### preamble
# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(dplyr)
library(lhs)
library(boot)
library(data.table)
library(rdist)
library(vroom)
library(logitnorm)
# library(TruncatedDistributions)
# library(foreach)
# library(doParallel)

# load morevac package
# setwd("~/Documents/morevac") # Mac path
# setwd("~/morevac") # PC path
devtools::load_all()
###

#######################################
# specify save directory
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data")
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
# setwd("C:/Users/ainsl/Dropbox/Kylie/Projects/Morevac/data/sim_data/")

#######################################
 ### Baseline
#######################################
# run simulations
run_sims_all(params_file = "param_values_baseline_time-varying_beta.csv", index = c(1,2,4,6,9,11), out_file = "sim_baseline2_")
#######################################
 ### Latin hypercube
#######################################
# create parameter combination
params <- create_params_file(n_sim = 500, n_indiv = 30000, lhc_size = 500, out_file = "param_values_10",
                             vac_cutoff = 10, seed = 1234)

# run simulations
run_sims_all(params_file = "param_values_10.csv", index = 121:140, out_file = "sim_10_")

#######################################
 ### Simulation results post-processing
#######################################
# read in parameter file
param_path <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/"

param_values <- read.csv(paste0(param_path,"param_values_baseline_time-varying_beta.csv"), header = TRUE)
names(param_values)[1] <- "Param_Index"

# set working directory where results files are located
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10")
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/sim500/time-varying_beta")

# read in results (rather than re-run simulations)

files_inf <- data.frame(file_name = list.files(pattern="*inf_hist.csv"),
                        file_name_for_sep = list.files(pattern="*inf_hist.csv")) %>%
  separate(col = file_name_for_sep, into = c("x", "cut_off", "param_index", "file_type", "y"), sep = "_") %>%
  mutate(param_index = as.numeric(param_index)) %>%
  arrange(param_index)

files_vac <- data.frame(file_name = list.files(pattern="*vac_hist.csv"),
                        file_name_for_sep = list.files(pattern="*vac_hist.csv")) %>%
  separate(col = file_name_for_sep, into = c("x", "cut_off", "param_index", "file_type", "y"), sep = "_") %>%
  select(-x,-y) %>%
  mutate(param_index = as.numeric(param_index)) %>%
  arrange(param_index)

all_files <- left_join(files_vac, files_inf, by = "param_index")
n_files <- dim(all_files)[1]

### create progress bar
start_index <- 1
for (i in start_index:n_files){
### read in data from list of files
  print(all_files$file_name.y[i])
  dt_inf = vroom(file = all_files$file_name.y[i], delim = ",", col_names = TRUE)  %>%
              rename("Vac_Strategy" = "Vac_Strategy...1",
                     "Vac_Strategy_num" = "Vac_Strategy...3") %>%
              mutate(Num_Infs = rowSums(select(.,Age0:Age18)), Param_Index = files_inf$param_index[i])

  print(all_files$file_name.x[i])
  dt_vac = vroom(file = all_files$file_name.x[i], delim = ",", col_names = TRUE) %>%
              rename("Vac_Strategy" = "Vac_Strategy...1",
                     "Vac_Strategy_num" = "Vac_Strategy...3") %>%
              mutate(Num_Vacs = rowSums(select(.,Age0:Age18)), Param_Index = files_vac$param_index[i])
  if(length(unique(dt_vac$Vac_Strategy))<3){next} # make sure vac data contains all three vac strategies

#######################################
# summarise raw data for childhood infs
  banana <- bind_cols(dt_inf[,c("Vac_Strategy", "Sim", "Cohort","Param_Index", "ID", "Num_Infs")], Num_Vacs = dt_vac[,c("Num_Vacs")]) %>%
                group_by(Vac_Strategy, Sim) %>% summarise(Mean_Infs = mean(Num_Infs))

#######################################
### Bootstrapping
#######################################

# bootstrap to get CI for childhood infs
  foo1 <- function(data, indices){
    dt<-data[indices,]
    mean(dt$Mean_Infs)
  }
  my_bootstrap <- plyr::dlply(banana, "Vac_Strategy", function(dat) boot(dat, foo1, R=100)) # boostrap for each set of param values
  my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
  banana_boat <- banana %>% group_by(Vac_Strategy) %>% summarise(Mean_Infs = mean(Mean_Infs)) %>%
                  mutate(Lower = my_ci[1,], Upper = my_ci[2,], Param_Index = all_files$param_index[i])

# Difference in childhood infs
  banana_split <- banana %>% spread(Vac_Strategy, Mean_Infs) %>%
                    mutate(Diff_AB = Annual - Biennial, Diff_AN = Annual - No_Vac, Diff_BN = Biennial - No_Vac) %>%
                    select(Sim, Diff_AB, Diff_AN, Diff_BN) %>%
                    gather(Type, Difference, Diff_AB:Diff_BN)

# bootstrap to get CI for Difference
  foo2 <- function(data, indices){
    dt<-data[indices,]
    mean(dt$Difference)
  }
  my_bootstrap <- plyr::dlply(banana_split, "Type", function(dat) boot(dat, foo2, R=100)) # boostrap for each set of param values
  my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

  banana_split2 <- banana_split %>% group_by(Type) %>% summarise(Mean_Diff = mean(Difference)) %>%
                      mutate(Lower = my_ci[1,], Upper = my_ci[2,], Param_Index = all_files$param_index[i])

# summarise raw data for attack rates
  chocolate_sprinkles <- dt_inf %>% group_by(Vac_Strategy, Sim, Cohort, Param_Index) %>% do(tail(.,1))
  chocolate_bar <- dt_inf %>% group_by(Vac_Strategy, Sim, Cohort, Param_Index) %>% select(-ID) %>% summarise_all(list(sum))
  chocolate_bar$ID <- chocolate_sprinkles$ID
  chocolate_sundae <- chocolate_bar %>% mutate_at(vars(Age0:Age18), funs(./ID)) %>%
                        select(Param_Index, Vac_Strategy, Sim, Cohort, Age0:Age18) %>% group_by(Param_Index, Vac_Strategy, Sim) %>%
                        summarise_at(vars(Age0:Age18), mean) %>% gather(Age, Attack_Rate, Age0:Age18) %>%
                        mutate(Age = as.numeric(str_remove(Age, 'Age')))

# bootstrap to get CI for ARs
  foo3 <- function(data, indices){
    dt<-data[indices,]
    mean(dt$Attack_Rate)
  }
  my_bootstrap <- plyr::dlply(chocolate_sundae, c("Param_Index","Vac_Strategy","Age"), function(dat) boot(dat, foo3, R=1000)) # boostrap for each set of param values
  my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

  chocolate_sundae2 <- chocolate_sundae %>% group_by(Param_Index, Vac_Strategy, Age) %>% summarise(Mean_AR = mean(Attack_Rate)) %>%
                          ungroup() %>% mutate(Lower = my_ci[1,], Upper = my_ci[2,])

  if(i > start_index) {
    banana_cream_pie <- bind_rows(banana_cream_pie,banana_split2)
    banana_bread <- bind_rows(banana_bread,banana_boat)
    chocolate_surprise <- bind_rows(chocolate_surprise, chocolate_sundae2)
  } else {banana_cream_pie <- banana_split2
          banana_bread <- banana_boat
          chocolate_surprise <- chocolate_sundae2
  }

  # remove dt_inf and dt_vac from memory
  rm(dt_inf)
  rm(dt_vac)

  # write out files in case for loop stops
  data.table::fwrite(banana_cream_pie, file = paste0("banana_cream_pie_tmp",start_index,".csv"), col.names = TRUE,
                     row.names = FALSE, sep = ",")
  data.table::fwrite(banana_bread, file = paste0("banana_bread_tmp",start_index,".csv"), col.names = TRUE,
                     row.names = FALSE, sep = ",")
  data.table::fwrite(chocolate_surprise, file = paste0("chocolate_surprise_tmp",start_index,".csv"), col.names = TRUE,
                     row.names = FALSE, sep = ",")


  }

#######################################
### Create summary output files

# bind columns with parameter values
banana_cream_pie <- left_join(banana_cream_pie, param_values, by = c("Param_Index"))
banana_bread <- left_join(banana_bread, param_values, by = c("Param_Index"))
chocolate_surprise <- left_join(chocolate_surprise, param_values, by = c("Param_Index"))

# write files to avoid having to read in raw data again
data.table::fwrite(banana_cream_pie, file = "banana_cream_pie_baseline2.csv", col.names = TRUE,
                   row.names = FALSE, sep = ",")
data.table::fwrite(banana_bread, file = "banana_bread_baseline2.csv", col.names = TRUE,
                   row.names = FALSE, sep = ",")
data.table::fwrite(chocolate_surprise, file = "chocolate_surprise_baseline2.csv", col.names = TRUE,
                   row.names = FALSE, sep = ",")

############################################
### Fully vaccinated individuals
# vac_max <- c(max(banana[banana$Vac_Strategy == "Annual",]$Num_Vacs), max(banana[banana$Vac_Strategy == "Biennial",]$Num_Vacs))
# banana_pancake <- banana %>%
#   filter((Vac_Strategy == 'Annual' & Num_Vacs == vac_max[1]) |
#            (Vac_Strategy == 'Biennial' & Num_Vacs == vac_max[2])) %>%
#   group_by(Vac_Strategy, Param_Index, Sim) %>%
#   summarise(Mean_Infs = mean(Num_Infs)) %>%
#   spread(Vac_Strategy, Mean_Infs) %>%
#   mutate(Difference = Annual - Biennial) %>%
#   filter(!is.na(Difference))
#
# # bootstrap
# my_bootstrap2 <- plyr::dlply(banana_pancake, "Param_Index", function(dat) boot(dat, foo, R=1000)) # boostrap for each set of param values
# my_ci2 <- sapply(my_bootstrap2, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
# my_ci2[vapply(my_ci2, is.null, logical(1))] <- list(c(NA, NA))
# my_ci2a <- data.frame(matrix(unlist(my_ci2), nrow=length(my_ci2), byrow=T)) #bind_rows(my_ci2)
#
# banana_pancake2 <- banana_pancake %>% group_by(Param_Index) %>% summarise(Mean_Diff = mean(Difference))
# banana_pancake2$Lower <- my_ci2a[,1]
# banana_pancake2$Upper <- my_ci2a[,2]
# banana_pancake2 <- left_join(banana_pancake2, param_values, by = c("Param_Index"))
# # add Diff_Color column for plotting
# banana_pancake2$Diff_Color <- ifelse(banana_pancake2$Upper < 0, '<0',
#                                    ifelse(banana_pancake2$Lower <=0 & banana_pancake2$Upper >=0, 'zero',
#                                           ifelse(banana_pancake2$Lower >0, '>0', 'something else')))
# banana_pancake2$Diff_Color <- ifelse(is.na(banana_pancake2$Upper) & banana_pancake2$Mean_Diff <0, '<0', banana_pancake2$Diff_Color)
# banana_pancake2$Diff_Color <- ifelse(is.na(banana_pancake2$Lower) & banana_pancake2$Mean_Diff >0, '>0', banana_pancake2$Diff_Color)
# randomly select fully vac individuals and plot exposure history

