library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


#Weight for generic_polls weighted by date
w <- 0.05

#Day the generic_polls were collected
polls_downloaded_date <- "10-7-2022"

#Day the code is being run
run_day_char <- "10/7/2022"


run_day_date <-  as.Date.character(run_day_char,"%m/%d/%Y")


today_date <- as.Date.character(run_day_char,"%m/%d/%Y")

today_char <- as.character(today_date)

today_dash <- gsub("/", "-", today_char)

#Reads csv files for the generic_polls 
generic_polls <- read_csv(eval(paste("~/R Stuff/PoliStat/polls/", polls_downloaded_date, "/generic_ballot_polls.csv", sep = "")))


generic_polls <- generic_polls %>% 
  select(end_date, sample_size, population, fte_grade, methodology, dem, rep, poll_id) %>% 
  filter(!is.na(fte_grade) | fte_grade != "F" | fte_grade != "D") %>% 
  filter(!is.na(sample_size)) %>% 
  mutate(two_party_tot = dem + rep,
         dem_two_party = dem/two_party_tot)

generic_polls <- generic_polls %>% 
  mutate(days_before = as.numeric(today_date - as.Date(end_date, "%m/%d/%y"))) %>% 
  mutate(date_weight = (exp(1)^((-1)*days_before*w))) %>% 
  filter(days_before > -1) %>% 
  mutate(variance = (dem_two_party*(1-dem_two_party))/sample_size,
         standard_deviation = sqrt(variance))

generic_polls <- generic_polls %>% 
  mutate(real_date_weight = date_weight/sum(generic_polls$date_weight),
         weighted_poll = real_date_weight*dem_two_party,
         weighted_var = real_date_weight*variance,
         yes = "yes")

center_and_variance <- generic_polls %>% 
  group_by(yes) %>% 
  summarize(weighted_polls = sum(weighted_poll),
            weighted_var = sum(weighted_var),
            weighted_sd = sqrt(weighted_var)) %>% 
  ungroup() %>%
  select(c(-1))

####



#Writes a csv file with the center and variance for national mood
write_csv(center_and_variance, eval(paste("~/R Stuff/PoliStat/Class Model/For Simulation/National_Mood/", today_dash, "_National_Mood.csv", sep = "")))
