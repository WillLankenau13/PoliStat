library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

forSim <- read_csv("~/R Stuff/PoliStat/Class Model/For Simulation/2022-10-26.csv")
newVar <- read_csv("~/R Stuff/PoliStat/Class Model/For_Blog_Posts/fromFred.csv") %>% 
  mutate(office = str_to_title(office))
state_po <- read_csv("~/R Stuff/PoliStat/Class Model/state_po.csv")

forSim <- forSim %>% 
  full_join(state_po, by = c("state" = "State")) 

colnames(forSim)[10] <- "state_po"
colnames(forSim)[5] <- "o_var"

forSim <- forSim %>% 
  full_join(newVar, by = c("state_po" = "state", "office")) %>% 
  filter(!is.na(BPI))

colnames(forSim)[11] <- "variance"

forSim <- forSim %>% 
  mutate(tot_var = o_var + variance*100) %>% 
  select(state:polls, tot_var, standard_deviation:last_30_polls) %>% 
  mutate(standard_deviation = sqrt(tot_var))

colnames(forSim)[5] <- "variance"

write_csv(forSim, "~/R Stuff/PoliStat/Class Model/For_Blog_Posts/Fred.csv")


