## A/B Testing 

library(tidyverse)
library(lubridate)
library(ggplot2)
library(broom)
library(dplyr)


# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Build plot
ggplot(click_data_sum, aes(x = `week(visit_date)`,y = conversion_rate))   +  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent)


# power analysis
install.packages("powerMediation")
library(powerMediation)



## Power Analysis in R
## Example: 

# total_sample_size <- SSizeLogisticBin(p1 = 0.2,
                            #           p2 = 0.3,
                            #           B = 0.5,
                            #           alpha = 0.05,
                            #           power = 0.8)

## Analyzing Results

?glm
# Computing conversion rate difference by month
# new_data_name <- data_name %>% 
#   mutate(month_text = month(date, label = TRUE)) %>% 
#   group_by(month_test, variable) %>% 
#   summarize(conversion_rate = mean(clicks))

# newer_data_name <- new_data %>% 
#   spread(variable, conversion_rate)

# Plot monthly summary


ggplot(eight_month_checkin_data_sum,
       aes(x = month_text,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point(size = 4) +
  geom_line(lwd = 1) +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")

## What is A/B Testing?

# A/B testing is the use of expeerimental design and statistics to compare two or more variants of a design
  # Uses of A/B testing
    # Conversion rates(e.g., cliciks or purchases)
    # Engagement(e.g., sharing or "likes")
    # Dropoff rate (e.g., dose the consumer continue to other linked webpages)
    # Time spemt on a website

str(viz_website_2017)

viz_website_2017 %>%
  group_by(month(visit_date)) %>%
  summarize(article_conversion_rate = mean(clicked_article))

# Compute 'like' click summary by month
viz_website_2017_like_sum <- viz_website_2017 %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot 'like' click summary by month
ggplot(viz_website_2017_like_sum,
       aes(x = month, y = like_conversion_rate, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)

# Plot comparison of 'like'ing and 'sharing'ing an article
ggplot(viz_website_2017_like_share_sum,
       aes(x = month, y = conversion_rate, color = action, group = action)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0.00, 1.00), labels = percent)

## Assumptions and Types of A/B testing

## Assumptions
    #1, Within group vs. Between Group
        # Within: each participant sees both conditions (usually have higher power)
        # Between: different groups of participants see different conditions
    #1. There should be nothing qualitatively different between the two groups

## Types of A/B testing

# A/B - compare a control and a test condition

# A/A - compares two groups of control conditions

# A/B/N - compares a control condition to any number of different test conditions

## Confounding Variables are elements of the environment that could affect your ability to find out the truth of an A/B experiment

# Compute conversion rates for A/A experiment
viz_website_2018_01_sum <- viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

viz_website_2018_01_sum

# Plot conversion rates for two conditions
ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = percent)

# Load library to clean up model outputs
library(broom)

# Run logistic regression
aa_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_01) %>%
  tidy()
aa_experiment_results

# Compute 'like' conversion rate by week and condition
viz_website_2018_02 %>%
  mutate(week = week(visit_date)) %>%
  group_by(week, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Compute 'like' conversion rate by if article published and condition
viz_website_2018_02 %>%
  group_by(article_published, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot 'like' conversion rates by date for experiment
ggplot(viz_website_2018_02_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = article_published,
           group = interaction(condition, article_published))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-02-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)


# Compute 'like' conversion rate and mean pageload time by day
viz_website_2018_03_sum <- viz_website_2018_03 %>%
  group_by(visit_date,condition) %>%
  summarize(mean_pageload_time = mean(pageload_time),
            like_conversion_rate = mean(clicked_like))

# Plot effect of 'like' conversion rate by pageload time
ggplot(viz_website_2018_03_sum,
       aes(x = mean_pageload_time, y = like_conversion_rate, color = condition)) +
  geom_point()

# Plot 'like' conversion rate by day
ggplot(viz_website_2018_03_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = pageload_delay_added,
           group = interaction(condition, pageload_delay_added))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)

library(pwr)
?pwr


## Common statistical tests for A/B testing

# logistic regression
# t.test for continuous independent variable

# Load package to clean up model outputs
library(broom)

# Run logistic regression
ab_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_04) %>%
  tidy()
ab_experiment_results

# Run t-test
ab_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_04)
ab_experiment_results

## Stopping Rules and Sequential Analysis

install.packages("gsDesign")

# Example:

# seq__analysis <- gsDesign(k =  ,
          #                 test.type =  , 
          #                 alpha =  ,
          #                 beta =  ,
          #                 sfu =    )

# Load package to run sequential analysis
library(gsDesign)

# Run sequential analysis
seq_analysis_3looks <- gsDesign(k = 3,
                                test.type = 1,
                                alpha = 0.05,
                                beta = 0.2,
                                sfu = "Pocock")

# Fill in max number of points and compute points per group and find stopping points
max_n <- 3000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis_3looks$timing
stopping_points

# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))

# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = mean_time_spent_homepage_sec,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge")

# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = like_conversion_rate,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1), labels = percent)


# Organize variables and run logistic regression
viz_website_2018_05_like_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one,
                           levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two,
                           levels = c("better", "amazing"))) %>%
  glm(clicked_like ~ word_one * word_two,
      family = "binomial",
      data = .) %>%
  tidy()
viz_website_2018_05_like_results

