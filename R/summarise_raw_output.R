### run simulations from parameter file ###
# created: 25/10/2020
# last modified: 25/10/2020

#' This function runs sims for each combination of parameter value from parameter input file creates
#' @param dt_inf data frame of infection histories
#' @param dt_vac data frame of vaccination histories
#' @param id index of parameter values
#' @return returns a list of 3 data frames:
#'         - mean number of infections by vaccination strategy
#'         - mean difference in number of infections by vaccination strategy
#'         - mean attack rates by age and vaccination strategy
#' @keywords morevac
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_remove
#' @importFrom plyr dlply
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom utils tail
#' @export

summarise_raw_output <- function(dt_inf,
                                 dt_vac,
                                 id){

# combine dt_inf and dt_vac and determine mean number of
# infections by vac strategy
banana <- bind_cols(dt_inf[,c("Vac_Strategy",
                              "Sim",
                              "Cohort",
                              "Param_Index",
                              "ID",
                              "Num_Infs")],
                    Num_Vacs = dt_vac[,c("Num_Vacs")]) %>%
  group_by(.data$Vac_Strategy, .data$Sim) %>%
  summarise(Mean_Infs = mean(.data$Num_Infs))

### Bootstrapping

# bootstrap to get CI for childhood infs
foo1 <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Mean_Infs)
}
my_bootstrap <- plyr::dlply(banana, "Vac_Strategy", function(dat) boot(dat, foo1, R=1000)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
banana_boat <- banana %>% group_by(Vac_Strategy) %>%
  summarise(Mean_Infs = mean(.data$Mean_Infs)) %>%
  mutate(Lower = my_ci[1,], Upper = my_ci[2,], Param_Index = id)

# Difference in childhood infs
banana_split <- banana %>%
  spread(Vac_Strategy, Mean_Infs) %>%
  mutate(Diff_AB = .data$Annual - .data$Biennial,
         Diff_AN = .data$Annual - .data$No_Vac,
         Diff_BN = .data$Biennial - .data$No_Vac) %>%
  select(.data$Sim, .data$Diff_AB, .data$Diff_AN, .data$Diff_BN) %>%
  gather(.data$Type, .data$Difference, Diff_AB:Diff_BN)

# bootstrap to get CI for Difference
foo2 <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Difference)
}
my_bootstrap <- plyr::dlply(banana_split, "Type", function(dat) boot(dat, foo2, R=1000)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

banana_split2 <- banana_split %>% group_by(Type) %>%
  summarise(Mean_Diff = mean(Difference)) %>%
  mutate(Lower = my_ci[1,], Upper = my_ci[2,], Param_Index = id)

# summarise raw data for attack rates
chocolate_sprinkles <- dt_inf %>%
  group_by(.data$Vac_Strategy, .data$Sim, .data$Cohort, .data$Param_Index) %>% do(tail(.,1))
chocolate_bar <- dt_inf %>%
  group_by(Vac_Strategy, Sim, Cohort, Param_Index) %>%
  select(-.data$ID) %>%
  summarise_all(list(sum))
chocolate_bar$ID <- chocolate_sprinkles$ID
chocolate_sundae <- chocolate_bar %>%
  mutate_at(vars(Age0:Age18), funs(./ID)) %>%
  select(Param_Index, Vac_Strategy, Sim, Cohort, Age0:Age18) %>%
  group_by(Param_Index, Vac_Strategy, Sim) %>%
  summarise_at(vars(Age0:Age18), mean) %>%
  gather(Age, Attack_Rate, Age0:Age18) %>%
  mutate(Age = as.numeric(str_remove(Age, 'Age')))

# bootstrap to get CI for ARs
foo3 <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Attack_Rate)
}
my_bootstrap <- plyr::dlply(chocolate_sundae, c("Param_Index","Vac_Strategy","Age"), function(dat) boot(dat, foo3, R=1000)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

chocolate_sundae2 <- chocolate_sundae %>% group_by(Param_Index, Vac_Strategy, Age) %>%
  summarise(Mean_AR = mean(Attack_Rate)) %>%
  ungroup() %>%
  mutate(Lower = my_ci[1,], Upper = my_ci[2,])

# output
rtn <- list(mean_infs = banana_boat,
            mean_diff = banana_split2,
            mean_ar = chocolate_sundae2)
return(rtn)

}
