library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


demographics <- read_csv("~/R Stuff/PoliStat/demographics.csv")
names(demographics)[51] <- 'house_2018'
names(demographics)[38] <- 'non_college_white'

dem_adj_r_squared <- read_csv("~/R Stuff/PoliStat/dem_adj_r_squared.csv")
dem_adj_r_squared <- dem_adj_r_squared %>% 
  select(intercept, r_squared, terms)

#   

model <- lm(house_2018 ~ nonreligiousPercent + nonreligiousPercent + medianage + Percentimmigrants + non_college_white, data = demographics)
summary(model)

new_adj_r_squared <- data.frame(intercept = toString(attr(model$terms , "term.labels")),
                                r_squared = summary(model)$adj.r.squared) %>% 
  mutate(
    terms = str_count(intercept, ",") + 1,
  ) 

dem_adj_r_squared <- rbind(dem_adj_r_squared, new_adj_r_squared) %>% 
  arrange(desc(r_squared))

# dem_adj_r_squared <- data.frame(intercept = "test",
#                             r_squared = 0,
#                             terms = 0)
# dem_adj_r_squared <- dem_adj_r_squared[-c(1), ]


i <- 2

while(i < nrow(dem_adj_r_squared)){
    if(near(dem_adj_r_squared$r_squared[i], dem_adj_r_squared$r_squared[i - 1])){
      dem_adj_r_squared <- dem_adj_r_squared[-c(i), ]
      i <- i - 1
    }
  i <- i + 1
}

write.csv(dem_adj_r_squared, "dem_adj_r_squared.csv")




