library(tidyverse)
library(wcde)
options(scipen = 999)

my_countries = readRDS("data/countries.RDS")
# prep UN data
# 8: Demand for family planning satisfied by any modern method (Percent) 
un = un_raw %>%
  filter(
    IndicatorId == 8,
    Category == "All women",
    Variant == "Median") %>% 
  mutate(un_mn = as.numeric(Value)) %>%
  select(country = Location,  year = Time, un_mn) 

saveRDS(un, "data/un_met_need.RDS")

# prep Wittgenstein data
# view(wic_indicators) 
my_indicators = c("epop", "prop", "bprop", "mys", "bmys")

# wcde_raw <- tibble(indicator = my_indicators) %>%
#   mutate(data = map(.x = indicator,  ~get_wcde(indicator = .x, country_name = my_countries$name)))

wcde = readRDS("data/wcde_raw.RDS")
bmys = wcde_raw %>%
  filter(indicator == "bmys") %>%
  unnest(data) %>%
  filter(age == "25+", sex == "Female") %>%
  mutate(name = ifelse(
    name == "United Kingdom of Great Britain and Northern Ireland",
    "United Kingdom", name)) %>%
  select(country = name, year, wcde_edu = bmys)  

saveRDS(bmys, "data/wcde_education.RDS")


