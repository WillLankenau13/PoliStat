library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

a <- 8/pi
b <- 0.2

Goofi <- read_csv("~/R Stuff/PoliStat/Class Model/for_goofi.csv")

names(Goofi)[1] <- 'state'

Goofi <- Goofi %>% 
  mutate(percent_error = 100*(D - Oracle)/Oracle)

Goofi <- Goofi %>% 
  mutate(variance = (a*atan(b*(abs(percent_error))))^2)

write_csv(Goofi, "~/R Stuff/PoliStat/Class Model/goofi.csv")

