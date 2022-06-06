library(tidyverse)
library(HMDHFDplus)
library(countrycode)

#####
# READ FERTILITY DATA
tfr_raw = read_csv("data/IHME_GBD_2019_FERTILITY/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M10D27.CSV")
ccfr_raw = readHFD("data/hfd_ccfr/ccfrVH.txt")

# SELECT COUNTRIES 
countries = ccfr_raw %>% 
  filter(Age == 50 & !is.na(CCFR)) %>% 
  group_by(Code) %>% 
  summarise(
    begin = min(Cohort), 
    end = max(Cohort),
    lower = min(CCFR),
    upper = max(CCFR)) 

# view(countries)

my_countries = countries %>%
  filter(begin < 1933 & end > 1960) %>%
  select(code = Code) %>%
  mutate(name = countrycode(
    code, 
    warn = FALSE,
    origin = "iso3c", 
    destination = "country.name",
    custom_match = c(
      "GBRTENW" = "United Kingdom",
      "GBR_SCO" = "Scotland",
      "FRATNP" = "France")))

ccfr = ccfr_raw %>%
  rename_all(tolower) %>%
  filter(code %in% my_countries$code) %>%
  filter(cohort %in% 1932:1968) %>%
  filter(age == 50) %>%
  mutate(year = cohort + 50) %>% 
  left_join(my_countries, by = "code") %>%
  select(country = name, year, ccf50 = ccfr) %>%
  as_tibble()
  
combined = tfr_raw %>%
  select(country = location_name, year = year_id, tfr=val) %>%
  mutate(country = ifelse(
    country == "United States of America", "United States", country)) %>%
  right_join(ccfr, by = c("country", "year"))

#####
# READ COVARIATES
edu_raw = read_csv("data/IHME_GBD_2019_COVARIATES/IHME_GBD_2019_COV_1980_2019_MATERNAL_EDUC_YRS_PC_Y2020M07D31.CSV")
mn_raw = read_csv("data/IHME_GBD_2019_COVARIATES/IHME_GBD_2019_COV_1980_2019_CONTRA_DEMAND_SATISFIED_Y2020M07D31.CSV")

edu = edu_raw %>%
  select(country = location_name, year = year_id, edu = val) %>%
  mutate(country = ifelse(
    country == "United States of America", "United States", country))

mn = mn_raw %>%
  select(country = location_name, year = year_id, mn = val) %>%
  mutate(country = ifelse(
    country == "United States of America", "United States", country)) 

final = combined %>% 
  left_join(edu, by = c("country", "year")) %>% 
  left_join(mn, by = c("country", "year"))

saveRDS(final, "data/final.RDS")
  