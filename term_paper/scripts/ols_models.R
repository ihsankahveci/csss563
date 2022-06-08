library(tidyverse)
library(modelsummary)
library(splines)

source("scripts/logit_functions.R")
data = readRDS("data/cov_projections.RDS")
train = data %>% filter(pred == 0)
test = data %>% filter(pred == 1) %>% select(-tfr, -ccf50)

# Internal knot placements in the natural cubic spline were set to 
# 10th, 36.5th, and 75th percentiles, respectively, of education.
edu_knots = quantile(train$edu, probs = c(0.1, 0.365, 0.75))

m1 = lm(ccf50 ~ -1 + country, train)
m2 = lm(ccf50 ~ -1 + country + mn, train)
m3 = lm(ccf50 ~ -1 + country + mn + edu, train)
m4 = lm(ccf50 ~ -1 + country + mn + ns(edu), train)
m5 = lm(ccf50 ~ -1 + country + mn + ns(edu, df = 4, knots = edu_knots), train)

# comparing models and choosing the best fitting one
modelsummary(list(m1,m2,m3,m4,m5), statistic = NULL, fmt = 2,
             estimate = "{estimate} [{conf.low}, {conf.high}]", 
             coef_omit = "country")

best_model = m5

log_res = scaled_logit(train$ccf50, 1, 10) - 
  scaled_logit(best_model$fitted.values, 1, 10)

test$ccf50 = predict(best_model, newdata = test)

forecasts = bind_rows(test, train)

forecasts %>% 
  ggplot(aes(x = year, y=ccf50, linetype = pred)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~country) +
  theme_bw()


