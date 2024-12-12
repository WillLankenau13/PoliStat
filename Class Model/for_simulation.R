library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

####OUTDATED

State_lean <- read_csv("~/R Stuff/PoliStat/Class Model/State_lean.csv")
races <- read_csv("~/R Stuff/PoliStat/Class Model/races.csv")
state_pos <- read_csv("~/R Stuff/PoliStat/Class Model/state_po.csv")
Past_Results <- read_csv("~/R Stuff/PoliStat/Class Model/Past_Results_For_BPI.csv") 
Weighted_polls <- read_csv(eval(paste("~/R Stuff/PoliStat/Class Model/Weighted_polls/", today_dash, ".csv", sep = "")))
Goofi <- read_csv("~/R Stuff/PoliStat/Class Model/goofi.csv")


names(state_pos)[names(state_pos) == 'Code'] <- 'state_po'

races <- full_join(races, state_pos, by = c("state_po")) %>% 
  filter(!is.na(office)) %>% 
  select(State, office) %>% 
  mutate(race = "yes")

Weighted_polls <- Weighted_polls %>% 
  full_join(races, by = c("state" = "State", "office_type" = "office")) %>% 
  filter(race == "yes") %>% 
  mutate(last_30_polls = ifelse(is.na(last_30_polls), 0, last_30_polls),
         number_of_polls = ifelse(is.na(number_of_polls), 0, number_of_polls))

### FIX, TEMPORARY SOLUTION
#### SD with no polls
Weighted_polls <- Weighted_polls %>% 
  mutate(weighted_sd = ifelse(is.na(weighted_sd), 0.03, weighted_sd),
         weighted_var = ifelse(is.na(weighted_var), (weighted_sd)^2, weighted_var))

### Variables
a <- 1.72
b <- 0.6
c <- 0.05
und_a <- 0.6
und_b <- 0.6
und_c <- 0.05
  
  BPI <- Past_Results %>% 
    mutate(BPI = Past_Results) %>% 
    select(state, BPI)
  
  ### State_Lean ###
  State_lean <- full_join(BPI, Weighted_polls, by = c("state")) %>% 
    filter(race == "yes")
  
  State_lean <- State_lean[, c(1, 3, 2, 4:9)]
  
  
  ### Additional Variance ###
  State_lean <-  State_lean %>% 
    left_join(Goofi, by = c("state")) 
  
  State_lean <-  State_lean %>% 
    mutate(BPI = BPI*100,
           weighted_sd = weighted_sd*100,
           weighted_var = weighted_sd^2,
           sd  = sqrt(variance),
           variance = sd^2
  )
  
  State_lean <- State_lean %>% 
    mutate(percent_var_und = (und_a/pi)*atan(und_b*last_30_polls + und_c*number_of_polls),
           und_variance = (2*log(weighted_undecided))^2,
           extra_variance = ifelse(percent_var_und == 0, variance, percent_var_und*und_variance + (1-percent_var_und)*variance),
           extra_sd = sqrt(extra_variance))
  
  State_lean <-  State_lean %>% 
    mutate(new_variance = ifelse(is.na(weighted_var), extra_variance, weighted_var+extra_variance),
           new_sd = sqrt(new_variance),
           weighted_polls = weighted_polls*100) 
  
  for_python <- State_lean %>% 
    select(state, office_type, BPI, weighted_polls, new_variance, new_sd, number_of_polls, last_30_polls)
  
  names(for_python)[2] <- 'office'
  names(for_python)[4] <- 'polls'
  names(for_python)[5] <- 'variance'
  names(for_python)[6] <- 'standard_deviation'

write_csv(for_python,eval(paste("~/R Stuff/PoliStat/Class Model/For Simulation/", today_dash, ".csv", sep = "")))



