
workdays_each_year <- 220                                                         # 220 average
manual_review_cost_per_year <- 50000                                              # annual salary
manual_review_cost_per_day <- manual_review_cost_per_year / workdays_each_year    # daily salary
manual_review_cost_per_minute <- manual_review_cost_per_day / (8 * 60)            # salary by minute
minutes_per_review <- 5                                                           # minutes each review takes

cost_of_false_positive <- minutes_per_review * manual_review_cost_per_minute
