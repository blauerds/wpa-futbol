library(worldfootballR)
library(tidyverse)
library(dplyr)
library(broom)


# Extract the data
big5_match_xG_data <- fb_match_results(country = c("ITA", "ENG", "FRA", "SPA", "GER"),
                                gender = "M",
                                season_end_year = c(2017:2023),
                                tier = "1st")

# Create the result and xG_diff columns
wrangled_big5_match_xG_data <- big5_match_xG_data %>%
  mutate(result = case_when(HomeGoals > AwayGoals ~ 1,
                                                         HomeGoals <= AwayGoals ~ 0,
                                                         HomeGoals == AwayGoals ~ 0)) %>%
  summarise(xG_1 = Home_xG, xG_2 = Away_xG, result = result) %>%
  mutate(xG_diff = xG_1 - xG_2)


# Fit a logistic regression model to predict the probability of winning based on the xG difference
logit_model <- glm(result ~ xG_diff, data = wrangled_big5_match_xG_data, family = binomial, na.action = na.omit)


# Use the coefficients from the logistic regression model to calculate the effect of each 0.01 xG
logit_coef <- tidy(logit_model, exponentiate = TRUE)

xG_effect <- logit_coef %>%
  filter(term == "xG_diff") %>%
  summarize(effect = exp(0.01 * estimate) - 1)


# Calculate the standard error of the xG_diff coefficient in the logistic regression model
xG_se <- logit_coef %>%
  filter(term == "xG_diff") %>%
  summarize(se = exp(estimate) * sqrt(vcov(logit_model)[2, 2]))


# Test the correlation between xG and win prob
correlation_test <- cor.test(wrangled_big5_match_xG_data$xG_diff,
                             wrangled_big5_match_xG_data$result,
                             use = "pairwise.complete.obs")
