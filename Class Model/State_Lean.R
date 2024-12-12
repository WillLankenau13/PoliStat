library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

a <- 1.72
b <- 0.6
c <- 0.05

BPI <- read_csv("~/R Stuff/PoliStat/Class Model/BPI.csv")
Weighted_polls <- read_csv("~/R Stuff/PoliStat/Class Model/Weighted_polls.csv")


State_lean <- full_join(BPI, Weighted_polls, by = c("state"))

State_lean <- State_lean[, c(1, 3, 2, 4:9)]


State_lean <- State_lean %>% 
  mutate(percent_polls = (a/pi)*atan(b*last_30_polls + c*number_of_polls),
         lean = percent_polls*weighted_polls + (1-percent_polls)*BPI)


write_csv(State_lean, "~/R Stuff/PoliStat/Class Model/State_lean.csv")




