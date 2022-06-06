library(tidyverse)
library(splines)
library(fable)

edu_raw = read_csv("data/IHME_GBD_2019_COVARIATES/IHME_GBD_2019_COV_1980_2019_MATERNAL_EDUC_YRS_PC_Y2020M07D31.CSV")
mn_raw = read_csv("data/IHME_GBD_2019_COVARIATES/IHME_GBD_2019_COV_1980_2019_CONTRA_DEMAND_SATISFIED_Y2020M07D31.CSV")
tfr_raw = read_csv("data/IHME_GBD_2019_FERTILITY/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M10D27.CSV")


european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czechia","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden","United Kingdom")



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

all_hts = as_tsibble(all, key = country, index = year) %>%
  aggregate_key(country, tfr = mean(tfr), edu = mean(edu), mn = mean(mn))
train = all_hts %>% filter(year < 2010)
test  = all_hts %>% filter(year >= 2010) 

all_hts %>%
  filter(!is_aggregated(country)) %>%
  autoplot(tfr) +
  labs(y = "TFR ", x = "",
       title = "IHME Estimates: European Union") +
  geom_vline(xintercept=2010, linetype = "dashed") + 
  geom_hline(yintercept = 2.1, linetype = "dotted") + 
  facet_wrap(~country, ncol = 4) +
  theme_bw() + 
  theme(legend.position = "none") 

ggsave("ihme_tfr.pdf", height = 14, width = 14)


fits = train %>% 
  model(base = ARIMA(tfr ~ pdq(0,1,0) + PDQ(0,0,0)),
        cov = ARIMA(tfr ~ mn + edu + pdq(0,1,0) + PDQ(0,0,0))) %>%
  reconcile(
    bu_base = bottom_up(base),
    bu_cov = bottom_up(cov),
    mint_base = min_trace(base, method = "mint_shrink"),
    mint_cov = min_trace(cov, method = "mint_shrink"))

fc_hts = fits %>% forecast(new_data = select(test, -tfr))

fc_hts %>%
  filter(!.model %in% c("base", "cov")) %>%
  # select(country %in% c("Bulgaria", "Sweden", "France", "Germany")) %>%
  filter(!is_aggregated(country)) %>%
    autoplot(
      all_ts,
      level = NULL) +
      geom_vline(xintercept=2010, linetype = "dashed") + 
      geom_hline(yintercept = 2.1, linetype = "dotted") + 
      facet_wrap(~country, scales = "free_y", ncol = 4) +
      theme_bw() 

ggsave("ar_multilevel.pdf", height = 14, width = 14) 
