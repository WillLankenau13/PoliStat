library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")

####OUTDATED

for_sim <- read_csv(eval(paste("~/R Stuff/PoliStat/Class Model/For Simulation/", today_dash, ".csv", sep = "")))
national_mood <- read_csv(eval(paste("~/R Stuff/PoliStat/Class Model/For Simulation/National_Mood/", today_dash, "_National_Mood.csv", sep = "")))

### FIX, TEMPORARY SOLUTION
#### SD with no polls
for_sim <- for_sim %>% 
  mutate(standard_deviation = ifelse(is.na(standard_deviation), 0.03, standard_deviation),
         polls = polls*100) 

### Variables
simulation_count <- 300
NationalMood <- c(center_and_variance$weighted_polls[1], center_and_variance$weighted_sd[1])
a <- 1.9
b <- 0.6
c <- 0.05

sim_count <- 0
         
Wins <- races %>% 
  select(State, office) %>% 
  filter(!is.na(office)) %>% 
  mutate(dem_wins = 0)

Democrat_Seats_Won <- data.frame(matrix(ncol = 2, nrow = 0))
column_names <- c("Sim_Count", "Seats_Won")
colnames(Democrat_Seats_Won) <- column_names

while(sim_count < simulation_count){
  sim <- for_sim
  sim_count = sim_count + 1
  print(sim_count)
  ### BPI ###
  National_Mood_Mean <- NationalMood[1] - 0.5
  National_Mood_SD <- sqrt(NationalMood[2])
  
  National_Mood_Weight <- 1
  
  Mood_Shift <- (rnorm(1, National_Mood_Mean, National_Mood_SD))*0.01*National_Mood_Weight
  
  sim <- sim %>% 
    mutate(BPI = BPI + Mood_Shift)
  
  
  ### State_Lean ###
  sim <- sim %>% 
    mutate(percent_polls = (a/pi)*atan(b*last_30_polls + c*number_of_polls),
           lean = ifelse(percent_polls == 0, BPI, percent_polls*polls + (1-percent_polls)*BPI))
  
  ### Simulation ###
  
  For_simulation <- sim %>% 
    select(state, office, standard_deviation, lean)
  
  Simulation <- For_simulation %>%
    mutate(lean = lean,
           standard_deviation = standard_deviation) %>% 
    mutate(simulated = rnorm(71, lean, standard_deviation)) %>% 
    mutate(difference = lean - simulated)
  
  
  ### Correlation ###
  
  
  
  ### Wins ###
  Simulation <- Simulation %>% 
    mutate(Win = ifelse(simulated > 50, 1, 0)) %>% 
    filter(!is.na(office))
  
  Wins <- full_join(Wins, Simulation, by = c("State" = "state", "office" = "office")) %>% 
    mutate(dem_wins = dem_wins + Win) %>% 
    select(State, office, dem_wins)
  
  Senate <- Simulation %>% 
    filter(office == "Senate")
  
  Dem_Seats_Won = sum(Senate[7])
  
  This_Simulation_Seats_Won <- data.frame(sim_count, Dem_Seats_Won)
  
  Democrat_Seats_Won = rbind(Democrat_Seats_Won, This_Simulation_Seats_Won)
  
}


Democrat_Seats_Won_Total <- Democrat_Seats_Won %>% 
  group_by(Dem_Seats_Won) %>% 
  summarize(count = n()) %>% 
  mutate(count/simulation_count) 


Sample <- Wins %>% 
  full_join(sim, by = c("State" = "state", "office" = "office")) %>% 
  full_join(state_pos, by = c("State")) %>% 
  select(state_po, office, BPI, polls, standard_deviation, variance, lean, dem_wins) %>% 
  mutate(state_po = ifelse((office == "Senate 1"), "OK", state_po),
         state_po = ifelse((office == "Senate 2"),"OK2", state_po),
        office = ifelse((office == "Senate 1" | office == "Senate 2"), "Senate", office)) %>% 
  filter(!is.na(dem_wins))
  

write_csv(Sample ,"~/R Stuff/PoliStat/Class Model/Sample.csv")
  
  
  