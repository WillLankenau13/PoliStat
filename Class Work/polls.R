library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")



polls <- read_csv("~/R Stuff/PoliStat/Past_Election_Results/1976_2020_senate.csv")
results <- read_csv("~/R Stuff/1976_2020_results.csv") %>% 
  filter(year == "2018" | year == "2020") %>% 
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>% 
  mutate(district = as.double(district),
         state = str_to_title(state),
         party = substr(party, 1, 3)
         )

combined <- left_join(polls, results, by = c("cycle" = "year", "state", "seat_number" = "district", "candidate_party" = "party")) %>% 
  filter(!is.na(state_po)) 

two_party_votes <-  combined %>% 
  group_by(race_id) %>% 
  summarise(two_party_votes = 2*sum(candidatevotes)/n())

combined <- left_join(combined, two_party_votes, by = c("race_id")) %>% 
  filter(candidate_party == "DEM") %>% 
  mutate(two_party_percent_dem = 100*candidatevotes/two_party_votes)

combined <- combined %>% 
  mutate(difference = pct - two_party_percent_dem) %>% 
  filter(difference > -30)

names(combined)[16] <- 'days_before_election'

combined <- combined %>% 
  filter(days_before_election < 365) %>% 
  filter(days_before_election > -1)

sample_combined <- combined[sample(nrow(combined), 800), ]

ggplot(data = combined, mapping = aes(x = reorder(fte_grade, difference, FUN = median), y = difference)) +
  geom_boxplot()

by_poll_grade <- sample_combined %>% group_by(fte_grade) %>%
  summarize(mean = mean(difference),
            sd = sd(difference),
            first_quartile = quantile(difference, 0.25),
            median = median(difference),
            third_quartile = quantile(difference, 0.75),
            count = n())

model <- lm(difference ~ days_before_election + sample_size + population + totalvotes + fte_grade, data = sample_combined)
summary(model)


sample_combined <- sample_combined %>% 
  add_predictions(model) %>% 
  add_residuals(model)

ggplot(sample_combined, aes(days_before_election, difference), position = "jitter")  +
  geom_jitter()

ggplot(data = combined, mapping = aes(x = reorder(population, difference, FUN = median), y = difference)) +
  geom_boxplot()

by_poll_population <- sample_combined %>% group_by(population) %>%
  summarize(mean = mean(difference),
            sd = sd(difference),
            first_quartile = quantile(difference, 0.25),
            median = median(difference),
            third_quartile = quantile(difference, 0.75),
            count = n())

 ggplot(data = combined) +
   geom_histogram(mapping = aes(x = days_before_election), bins = 100)
   
 
 ggplot(sample_combined, aes(sample_size, difference), position = "jitter")  +
   geom_jitter()
 
 
 ggplot(sample_combined, aes(totalvotes, difference), position = "jitter")  +
   geom_jitter()
 
 
 
 
 