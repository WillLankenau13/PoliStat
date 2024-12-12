library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

senate_polls <- senate_polls_down
governor_polls <- governor_polls_down

#Combines polls into one df
polls <- rbind(senate_polls, governor_polls) %>% 
  select(state, office_type, end_date, sample_size, population, fte_grade, methodology, candidate_name, party, pct, race_id, poll_id) %>% 
  filter(!is.na(fte_grade) | fte_grade != "F" | fte_grade != "D") %>% 
  mutate(party = ifelse(candidate_name == "Evan McMullin", "DEM", party))

Louisiana_polls <- polls %>% 
  filter(state == "Louisiana")

### TEMPORARY SOLUTION
#Louisiana REP vs any DEM

pct_sum_per_poll <- Louisiana_polls %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            two_party_tot = sum(pct)) 

Louisiana_polls <- Louisiana_polls %>% 
  filter(party == "REP") %>% 
  full_join(pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
  mutate(two_party_sample = (two_party_tot/100)*sample_size)

third_party_pct <- Louisiana_polls %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            undecided = 100 - two_party_tot) 

Louisiana_polls <- Louisiana_polls %>% 
  full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 


Louisiana_polls <- Louisiana_polls %>% 
  mutate(two_party_pct = pct/two_party_tot) %>%
  mutate(variance = two_party_pct*(1 - two_party_pct)/two_party_sample,
         standard_deviation = sqrt(variance))

Louisiana_polls <- Louisiana_polls %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

summary <- Louisiana_polls %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

Louisiana_polls <- Louisiana_polls %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(real_date_weight = date_weight/sum,
         weighted_poll = real_date_weight*two_party_pct,
         weighted_sd = real_date_weight*standard_deviation,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

Louisiana_date_weighted_polls_by_race <- Louisiana_polls %>% 
  group_by(race_id, state, office_type) %>% 
  summarize(weighted_polls = 1 - sum(weighted_poll),
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var),
            weighted_undecided = sum(weighted_undecided),
            number_of_polls = sum(number_of_polls)/n(),
            last_30_polls = sum(last_30)) %>% 
  ungroup() %>%
  select(c(-1)) %>% 
  mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))

