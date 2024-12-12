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

Alaska_polls <- polls %>% 
  filter(state == "Alaska") %>% 
  filter(candidate_name == "Mike Dunleavy" | candidate_name == "Les Gara" | candidate_name == "Charlie Pierce" | 
           candidate_name == "Bill Walker" | candidate_name == "Lisa Murkowski" | candidate_name == "Kelly C. Tshibaka" | 
           candidate_name == "Patricia R. Chesbro") %>% 
  mutate(party = ifelse(candidate_name == "Bill Walker", "DEM", party))

Alaska_senate <- Alaska_polls %>% 
  filter(office_type == "U.S. Senate")

Alaska_governor <- Alaska_polls %>% 
  filter(office_type == "Governor") %>% 
  mutate(round = NA)

###



l <- list(Alaska_governor$candidate_name[1])
c <- 2

while(c <= nrow(Alaska_governor)){
  
  if(Alaska_governor$candidate_name[c] %in% l | nrow(Alaska_governor) == c | !identical(Alaska_governor$poll_id[c],Alaska_governor$poll_id[c - 1])){
    print(c)
    print(length(l))
    if(length(l) == 4){
      Alaska_governor$round[c-1] <- 1
      Alaska_governor$round[c-2] <- 1
      Alaska_governor$round[c-3] <- 1
      Alaska_governor$round[c-4] <- 1
    } else if(length(l) == 3){
      Alaska_governor$round[c-1] <- 2
      Alaska_governor$round[c-2] <- 2
      Alaska_governor$round[c-3] <- 2
    } else if(length(l) == 2){
      Alaska_governor$round[c-1] <- 3
      Alaska_governor$round[c-2] <- 3
    } 
    
    if(nrow(Alaska_governor) == c){
      if(length(l) == 3){
        Alaska_governor$round[c] <- 1
        Alaska_governor$round[c-1] <- 1
        Alaska_governor$round[c-2] <- 1
        Alaska_governor$round[c-3] <- 1
      } else if(length(l) == 2){
        Alaska_governor$round[c] <- 2
        Alaska_governor$round[c-1] <- 2
        Alaska_governor$round[c-2] <- 2
      } else if(length(l) == 1){
        Alaska_governor$round[c] <- 3
        Alaska_governor$round[c-1] <- 3
      } 
    } else{
      assign("l", NULL, envir = .GlobalEnv)
      l <- list(Alaska_governor$candidate_name[c])
    }
    
  } else {
      l <- append(l, Alaska_governor$candidate_name[c])
  }
  
  c <- c + 1
}

Alaska_Governor_Round_1 <- Alaska_governor %>% 
  filter(round == 1)

Alaska_Governor_Round_2 <- Alaska_governor %>% 
  filter(round == 2)

Alaska_Governor_Round_3 <- Alaska_governor %>% 
  filter(round == 3)

Alaska_Governor_Round_1_grouped <- Alaska_Governor_Round_1 %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            party_tot = sum(pct)) 

Alaska_Governor_Round_1 <- Alaska_Governor_Round_1 %>% 
  full_join(Alaska_Governor_Round_1_grouped, by = c("poll_id", "state", "office_type")) %>% 
  mutate(sample = (party_tot/100)*sample_size)

Alaska_Governor_Round_1 <- Alaska_Governor_Round_1 %>% 
  group_by(poll_id, state, office_type) %>% 
  mutate(undecided = 100 - party_tot) 

Alaska_Governor_Round_1 <- Alaska_Governor_Round_1 %>% 
  mutate(real_pct = pct/party_tot) %>%
  mutate(variance = real_pct*(1 - real_pct)/sample,
         standard_deviation = sqrt(variance))

Alaska_Governor_Round_1 <- Alaska_Governor_Round_1 %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

Alaska_Governor_Round_1_summary <- Alaska_Governor_Round_1 %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

Alaska_Governor_Round_1 <- Alaska_Governor_Round_1 %>% 
  full_join(Alaska_Governor_Round_1_summary, by = c("race_id")) %>% 
  mutate(real_date_weight = 4*date_weight/(sum(Alaska_Governor_Round_1$date_weight)),
         weighted_poll = real_date_weight*real_pct,
         weighted_sd = real_date_weight*standard_deviation,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

Alaska_Governor_round_1_date_weighted_polls_by_candidate <- Alaska_Governor_Round_1 %>% 
  group_by(race_id, state, office_type, candidate_name) %>% 
  summarize(weighted_polls = sum(weighted_poll),
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var),
            weighted_undecided = sum(weighted_undecided),
            number_of_polls = sum(number_of_polls)/n(),
            last_30_polls = sum(last_30)) %>% 
  ungroup() %>% 
  select(c(-1)) %>% 
  mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))

Alaska_Governor_Round_2_grouped <- Alaska_Governor_Round_2 %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            party_tot = sum(pct)) 

Alaska_Governor_Round_2 <- Alaska_Governor_Round_2 %>% 
  full_join(Alaska_Governor_Round_2_grouped, by = c("poll_id", "state", "office_type")) %>% 
  mutate(sample = (party_tot/100)*sample_size)

Alaska_Governor_Round_2 <- Alaska_Governor_Round_2 %>% 
  group_by(poll_id, state, office_type) %>% 
  mutate(undecided = 100 - party_tot) 

Alaska_Governor_Round_2 <- Alaska_Governor_Round_2 %>% 
  mutate(real_pct = pct/party_tot) %>%
  mutate(variance = real_pct*(1 - real_pct)/sample,
         standard_deviation = sqrt(variance))

Alaska_Governor_Round_2 <- Alaska_Governor_Round_2 %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

Alaska_Governor_Round_2_summary <- Alaska_Governor_Round_2 %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

Alaska_Governor_Round_2 <- Alaska_Governor_Round_2 %>% 
  full_join(Alaska_Governor_Round_2_summary, by = c("race_id")) %>% 
  mutate(real_date_weight = 3*date_weight/(sum(Alaska_Governor_Round_2$date_weight)),
         weighted_poll = real_date_weight*real_pct,
         weighted_sd = real_date_weight*standard_deviation,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

Alaska_Governor_Round_2_date_weighted_polls_by_candidate <- Alaska_Governor_Round_2 %>% 
  group_by(race_id, state, office_type, candidate_name) %>% 
  summarize(weighted_polls = sum(weighted_poll),
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var),
            weighted_undecided = sum(weighted_undecided),
            number_of_polls = sum(number_of_polls)/n(),
            last_30_polls = sum(last_30)) %>% 
  ungroup() %>% 
  select(c(-1)) %>% 
  mutate(office_type = ifelse(office_type == "U.S. Senate", "Senate", office_type))

### TEMPORARY SOLUTION
#Alaska Senate between two REP
#Alaska Governor any REP vs DEM + IND
#Louisiana REP vs any DEM


### Alaska Senate ###

Alaska_senate <- Alaska_senate %>% 
  filter(party == "REP") 

Alaska_senate_pct_sum_per_poll <- Alaska_senate %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            rep_tot = sum(pct)/(count/2)) 

Alaska_senate <- Alaska_senate %>% 
  filter(candidate_name == "Lisa Murkowski") %>% 
  full_join(Alaska_senate_pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
  mutate(rep_party_sample = (rep_tot/100)*sample_size/(count/2))

third_party_pct <- Alaska_senate %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            undecided = 100 - rep_tot) 

Alaska_senate <- Alaska_senate %>% 
  full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 


Alaska_senate <- Alaska_senate %>% 
  mutate(two_party_pct = pct/rep_tot) %>%
  mutate(variance = two_party_pct*(1 - two_party_pct)/rep_party_sample)

Alaska_senate <- Alaska_senate %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

summary <- Alaska_senate %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

Alaska_senate <- Alaska_senate %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(real_date_weight = date_weight/sum,
         weighted_poll = real_date_weight*two_party_pct,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

Alaska_senate_date_weighted_polls_by_race <- Alaska_senate %>% 
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


### Alaska Governor ###

Alaska_governor_pct_sum_per_poll <- Alaska_governor %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            total = sum(pct)) 

Alaska_governor <- Alaska_governor %>% 
  filter(candidate_name == "Lisa Murkowski") %>% 
  full_join(Alaska_governor_pct_sum_per_poll, by = c("poll_id", "state", "office_type")) %>% 
  mutate(rep_party_sample = (rep_tot/100)*sample_size/(count/2))

third_party_pct <- Alaska_governor %>% 
  group_by(poll_id, state, office_type) %>% 
  summarize(count = n(),
            undecided = 100 - rep_tot) 

Alaska_governor <- Alaska_governor %>% 
  full_join(third_party_pct, by = c("poll_id", "state", "office_type")) 


Alaska_governor <- Alaska_governor %>% 
  mutate(two_party_pct = pct/rep_tot) %>%
  mutate(variance = two_party_pct*(1 - two_party_pct)/rep_party_sample)

Alaska_governor <- Alaska_governor %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(last_30 = ifelse(days_before < 31, 1, 0))

summary <- Alaska_governor %>%
  group_by(race_id) %>%
  summarize(number_of_polls = n(),
            sum = sum(abs(date_weight)))

Alaska_governor <- Alaska_governor %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(real_date_weight = date_weight/sum,
         weighted_poll = real_date_weight*two_party_pct,
         weighted_var = real_date_weight*variance,
         weighted_undecided = real_date_weight*undecided)

Alaska_governor_date_weighted_polls_by_race <- Alaska_governor %>% 
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


