library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("modelr")


polls <- read_csv("~/R Stuff/polls_governors.csv")
names(polls)[34] <- 'days_before_election'


# ggplot(data = polls, mapping = aes(x = reorder(population, error, FUN = median), y = error)) +
#   geom_boxplot()
# 
# ggplot(data = polls, mapping = aes(x = reorder(fte_grade, error, FUN = median), y = error)) +
#   geom_boxplot()
# 
# ggplot(data = polls, mapping = aes(x = reorder(methodology, error, FUN = median), y = error)) +
#   geom_boxplot()
# 
# by_poll_population <- polls %>% group_by(population) %>%
#   summarize(mean = mean(error),
#             sd = sd(error),
#             first_quartile = quantile(error, 0.25),
#             median = median(error),
#             third_quartile = quantile(error, 0.75),
#             count = n())
# 
# by_poll_grade <- polls %>% group_by(fte_grade) %>%
#   summarize(mean = mean(error),
#             sd = sd(error),
#             first_quartile = quantile(error, 0.25),
#             median = median(error),
#             third_quartile = quantile(error, 0.75),
#             count = n())
# 
# by_poll_methodology <- polls %>% group_by(methodology) %>%
#   summarize(mean = mean(error),
#             sd = sd(error),
#             first_quartile = quantile(error, 0.25),
#             median = median(error),
#             third_quartile = quantile(error, 0.75),
#             count = n())
# 
# 
# round_df <- function(x, digits) {
#   # round all numeric variables
#   # x: data frame 
#   # digits: number of digits to round
#   numeric_columns <- sapply(x, mode) == 'numeric'
#   x[numeric_columns] <-  round(x[numeric_columns], digits)
#   x
# }
# 
# by_poll_population <- round_df(by_poll_population, 3)
# by_poll_grade <- round_df(by_poll_grade, 3)
# by_poll_methodology <- round_df(by_poll_methodology, 3)
# 
# polls <- polls %>% 
#   mutate(ln_sample_size = ifelse(error != 0, log(abs(sample_size)), 0)) 
# 
# 
# ggplot(polls, aes(ln_sample_size, error), position = "jitter")  +
#   geom_jitter()
# 
# rv_lv_polls <- polls %>% 
#   filter(population == "rv" | population == "lv")
# 
# ggplot(rv_lv_polls, aes(sample_size, error), position = "jitter")  +
#   geom_jitter()
# 
# abs_polls <- polls %>% 
#   mutate(error = abs(error))
# 
# ggplot(abs_polls, aes(sample_size, error), position = "jitter")  +
#   geom_jitter()
# 
# model <- lm(error ~ sample_size, abs_polls)
# summary(model)
# 
# grouped <- polls %>% 
#   group_by(race_id) %>% 
#   summarize(count = n())
w = 0.0215

polls <- read_csv("~/R Stuff/polls_senate.csv")
names(polls)[14] <- 'days_before_election'

print(w)

polls <- polls %>% 
  mutate(weight = (exp(1)^((-1)*days_before_election*w)))



# polls <- polls %>%
#   mutate(weight_error = weights*error)
# weight_error = weight_act*error)

summary <- polls %>%
  group_by(race_id) %>%
  summarize(mean = mean(abs(weight)),
            count = n(),
            sum = sum(abs(weight)))

polls <- polls %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(weight_2 = weight/sum,
         weighted_poll = weight_2*`Two party %`,)

grouped2 <- polls %>% 
  group_by(race_id) %>% 
  summarize(weighted = sum(weighted_poll),
            actual = mean(`Actual 2 party`)) %>% 
  mutate(error = weighted - actual)

mean = mean(abs(grouped2$error))



ggplot(polls, aes(weight_2, error), position = "jitter")  +
  geom_jitter()

while(w < 0.022){
polls <- read_csv("~/R Stuff/polls_governors.csv")
names(polls)[34] <- 'days_before_election'

print(w)

polls <- polls %>% 
  mutate(weight = (exp(1)^((-1)*days_before_election*w)))



# polls <- polls %>%
#   mutate(weight_error = weights*error)
         # weight_error = weight_act*error)

summary <- polls %>%
  group_by(race_id) %>%
  summarize(mean = mean(abs(weight)),
            count = n(),
            sum = sum(abs(weight)))

polls <- polls %>% 
  full_join(summary, by = c("race_id")) %>% 
  mutate(weight_2 = weight/sum,
         weighted_poll = weight_2*`two party percentage`,)

grouped2 <- polls %>% 
  group_by(race_id) %>% 
  summarize(weighted = sum(weighted_poll),
            actual = mean(`actual two party percentage`)) %>% 
  mutate(error = weighted - actual)

mean = mean(abs(grouped2$error))



ggplot(polls, aes(weight_2, error), position = "jitter")  +
     geom_jitter()

means[nrow(means) + 1,] = list(w, mean)
w <- w + 0.0001
}

means <- means[-c(1), ]

# 0.01
# 0
# 0.1
# 0.05
# 0.02
# 0.015
# 0.025
# 0.021
# 0.022



# weight = 0.00
# 
# polls_7824 <- polls %>%
#   filter(race_id == 7824) %>%
#   mutate(weights = exp(1)^weight)
# sum = sum(polls_7824$weights)
# polls_7824 <- polls_7824 %>% 
#   mutate(weights = weights/sum)
# summary2 <- polls_7824 %>% 
#   group_by(party) %>% 
#   summarize(mean = mean(abs(error)),
#              sd = sd(abs(error)),
#              first_quartile = quantile(abs(error), 0.25),
#              median = median(abs(error)),
#              third_quartile = quantile(abs(error), 0.75),
#              count = n())
# 
# ggplot(polls_7824, aes(weight, error), position = "jitter")  +
#   geom_jitter()


# 0.01
# 0

p <- read_csv("~/R Stuff/polls_governors.csv")
names(p)[34] <- 'days_before_election'

w <- 0.02
shift <- -1000
count <- 0
while(w < 0.03){
      print(count)
  
  polls <- p %>% 
    mutate(weight = 1- (1/(1 + (exp(1)^((-1)*(days_before_election - shift)*w)))))
  
  
  
  # polls <- polls %>%
  #   mutate(weight_error = weights*error)
  # weight_error = weight_act*error)
  
  summary <- polls %>%
    group_by(race_id) %>%
    summarize(mean = mean(abs(weight)),
              count = n(),
              sum = sum(abs(weight)))
  
  polls <- polls %>% 
    full_join(summary, by = c("race_id")) %>% 
    mutate(weight_2 = weight/sum,
           weighted_poll = weight_2*`two party percentage`,)
  
  grouped2 <- polls %>% 
    group_by(race_id) %>% 
    summarize(weighted = sum(weighted_poll),
              actual = mean(`actual two party percentage`)) %>% 
    mutate(error = weighted - actual)
  
  logistic_mean = mean(abs(grouped2$error))
  
  
  
  ggplot(polls, aes(weight_2, error), position = "jitter")  +
    geom_jitter()
  
  l_mean[nrow(l_mean) + 1,] = list(w, shift, logistic_mean)
    x <- x + 1
  count <- count + 1
  w <- w + 0.00001
}


 l_mean <- data.frame(weight = -0.1,
                      shift = 0,
                             mean = 5)

