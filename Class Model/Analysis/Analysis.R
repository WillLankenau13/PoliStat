library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


our_sen_model <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/Our_model_11-3-2022.csv") %>% 
  filter(office == "Senate")
our_gov_model <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/Our_model_11-3-2022.csv") %>% 
  filter(office == "Governor")
five_thirty_eight_sen <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/senate_state_toplines_2022.csv") %>% 
  filter(expression == "_classic") %>% 
  filter(forecastdate == "11/2/22")
five_thirty_eight_gov <- read_csv("~/R Stuff/PoliStat/Class Model/Analysis/governor_state_toplines_2022.csv") %>% 
  filter(expression == "_classic") %>% 
  filter(forecastdate == "11/2/22")

five_thirty_eight_sen <- five_thirty_eight_sen %>% 
  mutate(state_po = substr(district, start = 1, stop = 2))

five_thirty_eight_sen <- five_thirty_eight_sen %>% 
  select(state_po, winner_D1, winner_R1, winner_I1, voteshare_mean_D1, voteshare_mean_R1, voteshare_mean_I1) %>% 
  mutate(winner_D1 = ifelse(winner_D1 == 0, winner_I1, winner_D1),
         voteshare_mean_D1 = ifelse(voteshare_mean_D1 == 0, voteshare_mean_I1, voteshare_mean_D1)) %>% 
  select(state_po, winner_D1, winner_R1, voteshare_mean_D1, voteshare_mean_R1) %>% 
  mutate(total_win = winner_D1 + winner_R1,
         total_per = voteshare_mean_D1 + voteshare_mean_R1,
         win_per = winner_D1/total_win,
         two_party_vote = voteshare_mean_D1/total_per) %>% 
  select(state_po, win_per, two_party_vote)

five_thirty_eight_gov <- five_thirty_eight_gov %>% 
  mutate(state_po = substr(district, start = 1, stop = 2))

five_thirty_eight_gov <- five_thirty_eight_gov %>% 
  select(state_po, winner_D1, winner_R1, winner_I1, voteshare_mean_D1, voteshare_mean_R1, voteshare_mean_I1) %>% 
  mutate(winner_D1 = ifelse(winner_D1 == 0, winner_I1, winner_D1),
         voteshare_mean_D1 = ifelse(voteshare_mean_D1 == 0, voteshare_mean_I1, voteshare_mean_D1)) %>% 
  select(state_po, winner_D1, winner_R1, voteshare_mean_D1, voteshare_mean_R1) %>% 
  mutate(total_win = winner_D1 + winner_R1,
         total_per = voteshare_mean_D1 + voteshare_mean_R1,
         win_per = winner_D1/total_win,
         two_party_vote = voteshare_mean_D1/total_per) %>% 
  select(state_po, win_per, two_party_vote)

combined_sen <- our_sen_model %>% 
  full_join(five_thirty_eight_sen, by = c("state_po")) %>% 
  mutate(difference = lean - 100*two_party_vote)

combined_gov <- our_gov_model %>% 
  full_join(five_thirty_eight_gov, by = c("state_po")) %>% 
  mutate(difference = lean - 100*two_party_vote)




#%>%
#   filter(state_po != "AK") %>% 
#   filter(state_po != "OK") %>% 
#   filter(state_po != "OK2") %>% 
#   mutate(our_z = qnorm((dem_wins)/100),
#          fte_z = qnorm(win_per),
#          our_fake_sd = (lean - 50)/our_z,
#          fte_fake_sd = (two_party_vote - 0.5)/fte_z) %>% 
#   mutate(difference = lean - two_party_vote*100)
# 
# 
# ggplot(combined, aes(lean, two_party_vote*100), position = "jitter") +
#   geom_jitter() +  
#   xlim(0, 100) +
#   ylim(0, 100) +
#   geom_abline(slope = 1, intercept = 0)
# 
# ggplot(combined, aes(dem_wins, win_per*100), position = "jitter") +
#   geom_jitter() +  
#   xlim(0, 100) +
#   ylim(0, 100) +
#   geom_abline(slope = 1, intercept = 0)
# 
# ggplot(combined, aes(lean, dem_wins), position = "jitter") +
#   geom_jitter() +  
#   xlim(0, 100) +
#   ylim(0, 100) +
#   geom_abline(slope = 1, intercept = 0)
# 
# ggplot(combined, aes(two_party_vote*100, win_per*100), position = "jitter") +
#   geom_jitter() +  
#   xlim(0, 100) +
#   ylim(0, 100) +
#   geom_abline(slope = 1, intercept = 0)
# 
# ggplot(combined, aes(our_fake_sd, fte_fake_sd), position = "jitter") +
#   geom_jitter() +  
#   geom_abline(slope = 1, intercept = 0)


