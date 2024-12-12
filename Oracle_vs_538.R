library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")



our_sen_model <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/Our_model_11-8-2022.csv") %>% 
  filter(office == "Senate")
our_gov_model <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/Our_model_11-8-2022.csv") %>% 
  filter(office == "Governor")
five_thirty_eight_sen <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/senate_state_toplines_2022.csv") %>% 
  filter(expression == "_classic") %>% 
  filter(forecastdate == "11/8/22")
five_thirty_eight_gov <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/governor_state_toplines_2022.csv") %>% 
  filter(expression == "_classic") %>% 
  filter(forecastdate == "11/8/22")

five_thirty_eight_sen <- five_thirty_eight_sen %>% 
  mutate(state_po = substr(district, start = 1, stop = 2))

five_thirty_eight_gov <- five_thirty_eight_gov %>% 
  mutate(state_po = substr(district, start = 1, stop = 2))

five_thirty_eight_sen <- five_thirty_eight_sen %>% 
  select(state_po, winner_D1, winner_R1, winner_I1, voteshare_mean_D1, voteshare_mean_R1, voteshare_mean_I1) %>% 
  mutate(winner_D1 = ifelse(winner_D1 == 0, winner_I1, winner_D1),
         voteshare_mean_D1 = ifelse(voteshare_mean_D1 == 0, voteshare_mean_I1, voteshare_mean_D1)) %>% 
  select(state_po, winner_D1, winner_R1, voteshare_mean_D1, voteshare_mean_R1) %>% 
  mutate(total_win = winner_D1 + winner_R1,
         total_per = voteshare_mean_D1 + voteshare_mean_R1,
         fte = winner_D1/total_win,
         two_party_vote = voteshare_mean_D1/total_per) %>% 
  select(state_po, fte)

five_thirty_eight_gov <- five_thirty_eight_gov %>% 
  select(state_po, winner_D1, winner_R1, winner_I1, voteshare_mean_D1, voteshare_mean_R1, voteshare_mean_I1) %>% 
  mutate(winner_D1 = ifelse(winner_D1 == 0, winner_I1, winner_D1),
         voteshare_mean_D1 = ifelse(voteshare_mean_D1 == 0, voteshare_mean_I1, voteshare_mean_D1)) %>% 
  select(state_po, winner_D1, winner_R1, voteshare_mean_D1, voteshare_mean_R1) %>% 
  mutate(total_win = winner_D1 + winner_R1,
         total_per = voteshare_mean_D1 + voteshare_mean_R1,
         fte = winner_D1/total_win,
         two_party_vote = voteshare_mean_D1/total_per) %>% 
  select(state_po, fte)

combined_sen <- our_sen_model %>% 
  full_join(five_thirty_eight_sen, by = c("state_po")) %>% 
  mutate(oracle = dem_wins/100) %>% 
  select(state_po, office, oracle, fte)

combined_gov <- our_gov_model %>% 
  full_join(five_thirty_eight_gov, by = c("state_po")) %>% 
  mutate(oracle = dem_wins/100) %>% 
  select(state_po, office, oracle, fte)


combined <- rbind(combined_sen, combined_gov)

write_csv(combined ,"~/R Stuff/PoliStat/Class Model/After_Election.csv")





