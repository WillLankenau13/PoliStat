library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

National_Mood_Mean <- NationalMood[1]
National_Mood_Var <- NationalMood[2]

National_Mood_Weight <- 1


Past_Results <- read_csv("~/R Stuff/PoliStat/Class Model/Past_Results_For_BPI.csv")


Shift <- (rnorm(1, National_Mood_Mean, National_Mood_Var))*0.01*National_Mood_Weight


BPI <- Past_Results %>% 
  mutate(BPI = Past_Results + Shift) %>% 
  select(state, BPI)


write_csv(BPI, "~/R Stuff/PoliStat/Class Model/BPI.csv")

