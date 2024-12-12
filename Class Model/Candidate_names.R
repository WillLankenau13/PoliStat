library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")



Senate <- read_csv("~/R Stuff/PoliStat/Class Model/538_Senate.csv") %>% 
  mutate(forecastdate = as.Date.character(forecastdate,"%m/%d/%y")) %>% 
  filter(forecastdate == as.Date.character("10/3/2022","%m/%d/%Y")) %>% 
  filter(expression == "_lite") %>% 
  select(-c("elasticity"))
Governor <- read_csv("~/R Stuff/PoliStat/Class Model/538_Governor.csv") %>% 
  mutate(forecastdate = as.Date.character(forecastdate,"%m/%d/%y")) %>% 
  filter(forecastdate == as.Date.character("10/3/2022","%m/%d/%Y")) %>% 
  filter(expression == "_lite")
state_pos <- read_csv("~/R Stuff/PoliStat/Class Model/state_po.csv")

names(state_pos)[names(state_pos) == 'Code'] <- 'state_po'

candidate_names <- rbind(Senate, Governor) %>% 
  select(branch, district, name_D1, name_R1, name_I1) %>%
  separate(district, "state_po") %>% 
  full_join(state_pos, by = c("state_po")) %>% 
  select(State, branch, name_D1, name_R1, name_I1) %>% 
  mutate(name_D1 = ifelse(is.na(name_D1), name_I1, name_D1)) %>% 
  select(State, branch, name_D1, name_R1) %>% 
  filter(!is.na(branch))

names(candidate_names)[1] <- 'state'
names(candidate_names)[2] <- 'office'
names(candidate_names)[3] <- 'democrat'
names(candidate_names)[4] <- 'republican'


write_csv(candidate_names, "~/R Stuff/PoliStat/Class Model/Candidate_names.csv")


