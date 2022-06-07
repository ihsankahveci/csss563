library(tidyverse)
library(modelsummary)
library(splines)
library(gtools)
library(fable)

data_raw = readRDS("data/final.RDS")

# data %>%
#   mutate(year = as_factor(year)) %>%
#   datasummary_skim(data = .)

data = data_raw 

# Internal knot placements in the natural cubic spline were set to 
# 10th, 36.5th, and 75th percentiles, respectively, of education.
edu_knots = quantile(data$edu, probs = c(0.1, 0.365, 0.75))

m1 = lm(ccf50 ~ 1, data)
m2 = lm(ccf50 ~ edu + mn, data)
m3 = lm(ccf50 ~ lag(ccf50), data)
m4 = lm(ccf50 ~ lag_ccf50, data)
m5 = lm(ccf50 ~ ns(edu, df = 4, knots = edu_knots) + mn, data)
m6 = lm(ccf50 ~ lag(ccf50) + ns(edu, df = 4, knots = edu_knots) + mn, data)


modelsummary(list(m1,m2,m3,m4,m5, m6), statistic = NULL, fmt = 2,
             estimate = "{estimate} [{conf.low}, {conf.high}]")

model = m5

res_df = tibble(
  res = model$residuals,
  log_res = logit(data$ccf50, 1, 10) - logit(model$fitted.values, 1, 10))



summary(m6)


ccf50_ts = ts(data$ccf50, start = 1982, end = 2018)
ccf50_cov = data %>% select(edu, mn) %>% as.matrix()

forecast::Arima(ccf50_ts, order = c(1,0,0), xreg = ccf50_cov, include.drift)


ccf50_ts = fable::as_tsibble(data, key = country, index = year)

fabletools::autoplot(ccf50_ts, ccf50, show.legend=FALSE) + 
  geom_line(aes(color = NA),show.legend=FALSE)+
  facet_wrap(~country) + 
  theme_bw()

models = ccf50_ts %>%
  fabletools::model(
    base = ARIMA(ccf50 ~ pdq(1,1,0) + PDQ(0, 0, 0)),
    cov = ARIMA(ccf50 ~ mn + edu + pdq(1,1,0) + PDQ(0, 0, 0)),
    ns = ARIMA(ccf50 ~ mn + ns(edu, df = 4) + pdq(1,1,0) + PDQ(0, 0, 0)))


models_BIC = glance(models) %>%
  select(country, .model, BIC) %>%
  pivot_wider(names_from = .model, values_from = BIC)
        
models_BIC %>% view()


glance(models) %>%
  select(country, .model, BIC) %>%
  ggplot(aes(y=country, x=BIC, color=.model)) + 
  geom_point(position = position_dodge2(width = 0.5)) +
  theme_bw()
