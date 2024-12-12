library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


pres_2012 <- 0
pres_2016 <- 0.033
pres_2020 <- 0.2

sen_2014 <- 0
sen_2016 <- 0.024
sen_2018 <- 0.11
sen_2020 <- 0.15

hou_2014 <- 0
hou_2016 <- 0.019
hou_2018 <- 0.06
hou_2020 <- 0.12

gov_2014 <- 0
gov_2016 <- 0.024
gov_2018 <- 0.11
gov_2020 <- 0.15


Past_years_house_results <- read_csv("~/R Stuff/PoliStat/Past_Election_Results/1976_2020_house.csv") %>% 
  filter(year == "2020" | year == "2018" | year == "2016" | year == "2014") %>% 
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>% 
  mutate(district = as.double(district),
         state = str_to_title(state),
         party = substr(party, 1, 3)
  ) %>% 
  select(state, office, district, stage, year, party, candidatevotes, totalvotes) 

Past_years_senate_results <- read_csv("~/R Stuff/PoliStat/Past_Election_Results/1976_2020_senate.csv") %>% 
  filter(year == "2020" | year == "2018" | year == "2016" | year == "2014") %>% 
  filter(party_detailed == "DEMOCRAT" | party_detailed == "REPUBLICAN") %>% 
  mutate(state = str_to_title(state),
         party = substr(party_detailed, 1, 3)
  ) %>% 
  select(state, office, district, stage, year, party, candidatevotes, totalvotes)

Past_years_president_results <- read_csv("~/R Stuff/PoliStat/Past_Election_Results/1976_2020_president.csv") %>% 
  filter(year == "2020" | year == "2018" | year == "2016" | year == "2012") %>% 
  filter(party_detailed == "DEMOCRAT" | party_detailed == "REPUBLICAN") %>% 
  mutate(state = str_to_title(state),
         party_detailed = substr(party_detailed, 1, 3),
         district = "statewide",
         stage = "gen",
         party = party_detailed
  ) %>% 
  select(state, office, district, stage, year, party, candidatevotes, totalvotes)

Past_years_governor_results <- read_csv("~/R Stuff/PoliStat/Past_Election_Results/1996_2020_governor.csv") %>% 
  filter(cycle == "2020" | cycle == "2018" | cycle == "2016" | cycle == "2014") %>% 
  filter(ballot_party == "DEM" | ballot_party == "REP") %>% 
  filter(stage == "general") %>% 
  mutate(office = office_name,
         district = "statewide",
         stage = "gen",
         year = cycle,
         party = ballot_party,
         candidatevotes = votes,
         totalvotes = candidatevotes*100/percent) %>% 
  select(state, office, district, stage, year, party, candidatevotes, totalvotes)


past_results <- rbind(Past_years_house_results, Past_years_president_results, Past_years_senate_results, Past_years_governor_results)

two_party_total <- past_results %>% 
  group_by(state, office, district, year) %>% 
  summarize(count = n(),
            two_party_votes = sum(candidatevotes))

past_results <- past_results %>%
  full_join(two_party_total, by = c("state", "office", "district", "year")) %>% 
  filter(party == "DEM") %>% 
  mutate(dem_percent = candidatevotes/two_party_votes,
         shizzam = year) %>% 
  mutate(office = ifelse(
    office == "US PRESIDENT", "President", ifelse(
      office == "US SENATE", "Senate", ifelse(
        office == "US HOUSE", "House", "Governor"
      )
    )
  ))

by_state <- past_results %>% 
  group_by(state, office, shizzam) %>% 
  summarize(two_party_state_votes = sum(two_party_votes),
            total_dem_votes = sum(candidatevotes))

by_state <- by_state %>% 
  mutate(year = as.character(shizzam)) %>% 
  mutate(dem_percent = total_dem_votes/two_party_state_votes)

by_state$year_office <- gsub(" ", "", paste(by_state$office, "_", by_state$year))

clean <- by_state %>% 
  ungroup() %>% 
  select(state, year_office, dem_percent)

visualize <- clean %>% 
  pivot_wider(names_from = year_office, values_from = dem_percent) 

visualize[visualize == 1] <- NA
visualize[visualize == 0.5] <- NA
visualize[visualize == 1/3] <- NA

visualize <- visualize[, c(1, 4:14, 2, 3, 15, 16)]
visualize <- visualize[,c(1, 6, 11, 2, 13, 7, 9, 3, 15, 12, 4, 14, 8, 10, 5, 16)]

weighted <- visualize %>% 
  mutate(president_2012_w = ifelse(is.na(President_2012), 0, pres_2012),
         president_2016_w = ifelse(is.na(President_2016), 0, pres_2016),
         president_2020_w = ifelse(is.na(President_2020), 0, pres_2020),
         senate_2014_w = ifelse(is.na(Senate_2014), 0, sen_2014),
         senate_2016_w = ifelse(is.na(Senate_2016), 0, sen_2016),
         senate_2018_w = ifelse(is.na(Senate_2018), 0, sen_2018),
         senate_2020_w = ifelse(is.na(Senate_2020), 0, sen_2020),
         house_2014_w = ifelse(is.na(House_2014), 0, hou_2014),
         house_2016_w = ifelse(is.na(House_2016), 0, hou_2016),
         house_2018_w = ifelse(is.na(House_2018), 0, hou_2018),
         house_2020_w = ifelse(is.na(House_2020), 0, hou_2020),
         governor_2014_w = ifelse(is.na(Governor_2014), 0, gov_2014),
         governor_2016_w = ifelse(is.na(Governor_2016), 0, gov_2016),
         governor_2018_w = ifelse(is.na(Governor_2018), 0, gov_2018),
         governor_2020_w = ifelse(is.na(Governor_2020), 0, gov_2020)
  ) 

weighted[is.na(weighted)] <- 0

weighted <- weighted %>% 
  mutate(original_weighted = president_2012_w*President_2012 + president_2016_w*President_2016 + president_2020_w*President_2020 +
           senate_2014_w*Senate_2014 + senate_2016_w*Senate_2016 + senate_2018_w*Senate_2018 + senate_2020_w*Senate_2020 +
           house_2014_w*House_2014 + house_2016_w*House_2016 + house_2018_w*House_2018 + house_2020_w*House_2020 + 
           governor_2014_w*Governor_2014 + governor_2016_w*Governor_2016 + governor_2018_w*Governor_2018 + governor_2020_w*Governor_2020) %>% 
  mutate(weights_sum = president_2012_w +
           president_2016_w +
           president_2020_w +
           senate_2014_w +
           senate_2016_w +
           senate_2018_w +
           senate_2020_w +
           house_2014_w +
           house_2016_w +
           house_2018_w +
           house_2020_w +
           governor_2014_w +
           governor_2016_w +
           governor_2018_w +
           governor_2020_w) %>% 
  mutate(adjusted_weighted = original_weighted/weights_sum)

Past_Results_Final <- weighted %>% 
  select(state, adjusted_weighted)

names(Past_Results_Final)[names(Past_Results_Final) == 'adjusted_weighted'] <- 'Past_Results'


write_csv(Past_Results_Final, "~/R Stuff/PoliStat/Class Model/Aileen_Past_Results_For_BPI.csv")




