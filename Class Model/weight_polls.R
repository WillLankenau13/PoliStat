library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

####OUTDATED


#Weight for polls weighted by date
w <- 0.05

#Day the polls were collected
polls_downloaded_date <- "10-1-2022"

#Day the code is being run
run_day_char <- "10/1/2022"


run_day_date <-  as.Date.character(run_day_char,"%m/%d/%Y")


today_date <- as.Date.character(run_day_char,"%m/%d/%Y")

today_char <- as.character(today_date)

today_dash <- gsub("/", "-", today_char)

#Reads csv files for the polls 
senate_polls <- read_csv(eval(paste("~/R Stuff/PoliStat/Polls/", polls_downloaded_date, "/senate_polls.csv", sep = ""))) %>% 
  filter(stage == "general")
governor_polls <- read_csv(eval(paste("~/R Stuff/PoliStat/Polls/", polls_downloaded_date, "/governor_polls.csv", sep = "")))  %>% 
  filter(stage == "general")

#Combines polls into one df
polls <- rbind(senate_polls, governor_polls) %>% 
  select(state, office_type, end_date, sample_size, population, fte_grade, methodology, candidate_name, party, pct, race_id, poll_id) %>% 
  filter(!is.na(fte_grade) | fte_grade != "F" | fte_grade != "D") %>% 
  mutate(party = ifelse(candidate_name == "Evan McMullin", "DEM", party))

Oklahoma_senate_polls <- polls %>% 
  filter(state == "Oklahoma") %>% 
  filter(office_type == "U.S. Senate")

Alaska_polls <- polls %>% 
  filter(state == "Alaska")

polls <- polls %>% 
  filter(!(state == "Oklahoma" & office_type == "U.S. Senate")) %>% 
  filter(state != "Alaska")

two_party_polls <- polls%>% 
  filter(party == "DEM" | party == "REP") 

pct_sum_per_poll <- two_party_polls %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            two_party_tot = sum(pct)) 

polls <- polls %>% 
  filter(party == "DEM") %>% 
  full_join(pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
  mutate(two_party_sample = (two_party_tot/100)*sample_size)

polls <- polls %>% 
  filter(count == 2)

third_party_pct <- polls %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            undecided = 100 - two_party_tot) 

polls <- polls %>% 
  full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 


polls <- polls %>% 
  mutate(two_party_pct = pct/two_party_tot) %>%
  filter(party == "DEM") %>% 
  mutate(variance = two_party_pct*(1 - two_party_pct)/two_party_sample,
         standard_deviation = sqrt(variance))

polls <- polls %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

summary <- polls %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

polls <- polls %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(real_date_weight = date_weight/sum,
         weighted_poll = real_date_weight*two_party_pct,
         weighted_sd = real_date_weight*standard_deviation,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

date_weighted_polls_by_race <- polls %>% 
  group_by(race_id, state, office_type) %>% 
  summarize(weighted_polls = sum(weighted_poll),
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var),
            weighted_undecided = sum(weighted_undecided),
            number_of_polls = sum(number_of_polls)/n(),
            last_30_polls = sum(last_30)) %>% 
  ungroup() %>%
  select(c(-1)) %>% 
  mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))

### Oklahoma ###

#Oklahoma is weird, its code is here

Oklahoma_senate_polls <- Oklahoma_senate_polls %>% 
  mutate(office_type = ifelse(race_id == 8943 , "Senate 1", "Senate 2"))

Oklahoma_senate_two_party_polls <- Oklahoma_senate_polls %>% 
  filter(party == "DEM" | party == "REP") %>% 
  mutate(office_type = ifelse(race_id == 8943 , "Senate 1", "Senate 2"))

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
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
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
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var),
            weighted_undecided = sum(weighted_undecided),
            number_of_polls = sum(number_of_polls)/n(),
            last_30_polls = sum(last_30)) %>% 
  ungroup() %>%
  select(c(-1)) %>% 
  mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))

####


#Weighted polls by date per race
date_weighted_polls_by_race <- rbind(date_weighted_polls_by_race, Oklahoma_senate_date_weighted_polls_by_race)


#Writes a csv file with the weighted polls
write_csv(date_weighted_polls_by_race, eval(paste("~/R Stuff/PoliStat/Class Model/Weighted_polls/", today_dash, ".csv", sep = "")))



