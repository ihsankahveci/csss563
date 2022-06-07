library(tidyverse)
library(countrycode)

un = readRDS("data/un_met_need.RDS")
wcde = readRDS("data/wcde_education.RDS")
final = readRDS("data/final.RDS")

combined = un %>%
  left_join(wcde, by = c("country", "year")) %>%
  left_join(final, by = c("country", "year")) %>% 
  arrange(country, year) %>%
  # interpolating wcde data using linear method
  mutate(wcde_edu = zoo::na.approx(wcde_edu, rule=2))

train = combined %>%
  filter(country %in% final$country, year %in% 1990:2018) 
  
test = combined %>% 
  filter(country %in% final$country, year > 2018) %>%
  select(country, year, un_mn, wcde_edu)

model_mn = lm(mn ~ un_mn + country, train)
summary(model_mn)
test$mn = predict(model_mn, newdata = test)

model_edu = lm(edu ~ wcde_edu + country, train)
summary(model_edu)
test$edu = predict(model_edu, newdata = test)


projections = bind_rows(test, train) %>% 
  mutate(pred = factor(ifelse(year > 2018, 1, 0)))

saveRDS(projections, "data/cov_projections.RDS")
  
  
  
  

       
  


