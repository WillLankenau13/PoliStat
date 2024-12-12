library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


w <- 0.05
today <-  as.Date.character("9/19/2022","%m/%d/%Y")

senate_polls <- read_csv("~/R Stuff/PoliStat/Polls/9-19-2022/senate_polls.csv") %>% 
  filter(party == "DEM" | party == "REP") %>% 
  filter(stage == "general")
governor_polls <- read_csv("~/R Stuff/PoliStat/Polls/9-19-2022/governor_polls.csv") %>% 
  filter(party == "DEM" | party == "REP") %>% 
  filter(stage == "general")

polls <- rbind(senate_polls, governor_polls) %>% 
  mutate(standard_deviation = 1/2 / sqrt(sample_size),
         variance = standard_deviation^2) %>% 
  select(state, office_type, end_date, sample_size, population, fte_grade, methodology, standard_deviation, variance, candidate_name, party, pct, race_id, poll_id)


pct_sum_per_poll <- polls %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            two_party_tot = sum(pct)) 

polls <- polls %>% 
  full_join(pct_sum_per_poll, by = c("poll_id", "state", "office_type"))

polls <- polls %>% 
  filter(count == 2)

polls <- polls %>% 
  mutate(two_party_pct = pct/two_party_tot) %>%
  filter(party == "DEM") %>% 
  filter(state == "New Mexico" & office_type == "Governor" | state == "Minnesota" & office_type == "Governor" | state == "Vermont" & office_type == "Governor" | state == "Connecticut" & office_type == "U.S. Senate")

polls <- polls %>% 
  mutate(days_before = as.numeric(today - as.Date(end_date, "%m/%e/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1)

summary <- polls %>%
  group_by(race_id) %>%
  summarize(count = n(),
            sum = sum(abs(date_weight)),
            sd_sum = sum(standard_deviation),
            sd_variance = sum(variance))

polls <- polls %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(real_date_weight = date_weight/sum,
         weighted_poll = real_date_weight*two_party_pct)

date_weighted_polls_by_race <- polls %>% 
  group_by(race_id, state, office_type) %>% 
  summarize(weighted_polls = sum(weighted_poll))



