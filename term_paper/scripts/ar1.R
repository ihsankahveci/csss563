library(tidyverse)
library(splines)
library(fable)

edu_raw = read_csv("data/IHME_GBD_2019_COVARIATES/IHME_GBD_2019_COV_1980_2019_MATERNAL_EDUC_YRS_PC_Y2020M07D31.CSV")
mn_raw = read_csv("data/IHME_GBD_2019_COVARIATES/IHME_GBD_2019_COV_1980_2019_CONTRA_DEMAND_SATISFIED_Y2020M07D31.CSV")
tfr_raw = read_csv("data/IHME_GBD_2019_FERTILITY/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M10D27.CSV")


european_union <- c("Austria","Belgium","Bulgaria",
                    "Czechia","Denmark","Estonia","Finland","France",
                    "Germany","Hungary","Switzerland","Italy",
                    "Lithuania","Netherlands",
                    "Portugal","Slovakia","Spain",
                    "Sweden")



edu = edu_raw %>%
  select(location_name, year_id, edu=val, upper, lower) 

mn = mn_raw %>%
  select(location_name, year_id, mn=val, upper, lower) 

tfr = tfr_raw %>%
  select(location_name, year_id, tfr=val, upper, lower) %>%
  filter(year_id %in% edu$year_id) %>%
  filter(location_name %in% edu$location_name)

all = tfr %>%
  left_join(edu, by = c("location_name", "year_id"), suffix = c("_tfr", "")) %>%
  left_join(mn, by = c("location_name", "year_id"), suffix = c("_edu", "_mn")) %>%
  filter(location_name %in% european_union) %>%
  rename(country = location_name, year = year_id)

all_ts = as_tsibble(all, key = country, index = year)
train = all_ts %>% filter(year < 2010)
test  = all_ts %>% filter(year >= 2010) 

# all_ts %>%
#   autoplot(tfr) +
#   labs(y = "TFR ", x = "",
#        title = "European Union") +
#   geom_vline(xintercept=2010, linetype = "dashed") + 
#   geom_hline(yintercept = 2.1, linetype = "dotted") + 
#   facet_wrap(~country, ncol = 4) +
#   theme_bw() + 
#   theme(legend.position = "none") 



fits = train %>% 
  model(
    base  = ARIMA(tfr ~ pdq(0,1,0) + PDQ(0,0,0)),
    cov =   ARIMA(tfr ~ mn + edu +  pdq(0,1,0) + PDQ(0,0,0)))
fc = fits %>% forecast(new_data = select(test, -tfr))

fc %>%
 # select(country %in% c("Bulgaria", "Sweden", "France", "Germany")) %>%
  autoplot(
    all_ts,
    level = NULL) +
  geom_vline(xintercept=2010, linetype = "dashed") + 
  geom_hline(yintercept = 2.1, linetype = "dotted") + 
  facet_wrap(~country, scales = "free_y", ncol = 4) +
  theme_bw() 


ggsave("ar_models.pdf", height = 14, width = 14)
