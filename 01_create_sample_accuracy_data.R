options(scipen=999)

library(tidyverse)
library(scales)

workdays_each_year <- 220                                                         # 220 average
manual_review_cost_per_year <- 100000                                             # annual salary/benefits etc
manual_review_cost_per_day <- manual_review_cost_per_year / workdays_each_year    # daily salary
manual_review_cost_per_minute <- manual_review_cost_per_day / (8 * 60)            # salary by minute
minutes_per_review <- 5                                                           # minutes each review takes

cost_per_false_positive <- minutes_per_review * manual_review_cost_per_minute

cost_per_false_negative <- 50 # Cost of each missed positive

df <- read_csv("process_name,true_positives,false_negatives,false_positives,true_negatives
ideal_state,10000,0,0,90000
current_state,10000,0,90000,0
wide_net,9900,100,86150,3850
medium_net,7500,2500,40000,50000
narrow_net,2000,8000,2500,87500
goal,8000,2000,30000,60000") %>% 
  mutate(total_positives = true_positives + false_negatives,
         total_negatives = false_positives + true_negatives,
         total_reviews = total_positives + total_negatives,
         cost_of_false_positives = cost_per_false_positive * false_positives,
         cost_of_false_negatives = cost_per_false_negative * false_negatives,
         total_cost = dollar(cost_of_false_positives + cost_of_false_negatives))

