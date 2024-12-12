library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

# Oklahoma_senate_polls <- polls %>% 
#   filter(state == "Oklahoma") %>% 
#   filter(office_type == "U.S. Senate")

###

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  mutate(state = ifelse(race_id == 8943 , "Oklahoma 1", "Oklahoma 2"))

Oklahoma_senate_two_party_polls <- Oklahoma_senate_polls %>% 
  filter(party == "DEM" | party == "REP") %>% 
  mutate(state = ifelse(race_id == 8943 , "Oklahoma 1", "Oklahoma 2"))

Oklahoma_senate_pct_sum_per_poll <- Oklahoma_senate_two_party_polls %>% 
  group_by(poll_id, state, office_type, race_id) %>% 
  summarize(count = n(),
            two_party_tot = sum(pct)) 

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  filter(party == "DEM") %>% 
  full_join(Oklahoma_senate_pct_sum_per_poll, by = c("poll_id", "state", "office_type", "race_id")) %>% 
  mutate(two_party_sample = (two_party_tot/100)*sample_size)

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  filter(count == 2)

Oklahoma_senate_third_party_pct <- Oklahoma_senate_polls %>% 
  group_by(poll_id, state, office_type, race_id) %>% 
  summarize(count = n(),
            undecided = 100 - two_party_tot) 

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  full_join(Oklahoma_senate_third_party_pct, by = c("poll_id", "state", "office_type", "race_id")) 


Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  mutate(two_party_pct = pct/two_party_tot) %>%
  filter(party == "DEM") %>% 
  mutate(variance = two_party_pct*(1 - two_party_pct)/two_party_sample,
         standard_deviation = sqrt(variance))

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  mutate(days_before = as.numeric(today - as.Date(end_date, "%m/%e/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

Oklahoma_senate_summary <- Oklahoma_senate_polls %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  full_join(Oklahoma_senate_summary, by = c("race_id")) %>% 
  mutate(real_date_weight = date_weight/sum,
         weighted_poll = real_date_weight*two_party_pct,
         weighted_sd = real_date_weight*standard_deviation,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

Oklahoma_senate_date_weighted_polls_by_race <- Oklahoma_senate_polls %>% 
  group_by(race_id, state, office_type) %>% 
  summarize(weighted_polls = sum(weighted_poll),
            weighted_sd = sum(weighted_sd),
            weighted_var = sum(weighted_var),
            weighted_undecided = sum(weighted_undecided),
            number_of_polls = sum(number_of_polls)/n(),
            last_30_polls = sum(last_30)) %>% 
  ungroup() %>%
  select(c(-1)) %>% 
  mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))



