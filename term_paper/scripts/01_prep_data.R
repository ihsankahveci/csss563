
# Load libraries ----------------------------------------------------------

library(methods)
library(dplyr)
library(dembase)
library(demdata)


# Load data ---------------------------------------------------------------

combined_data <-
  readRDS("data/combined/phl_2015_all.RDS") %>%
  rename(
    age = age_group_name,
    location = province
  )


# Prep deaths -------------------------------------------------------------

death_counts <- combined_data %>%
  select(-population) %>%
  xtabs(deaths ~ age + sex + location, data = .) %>%
  Counts() %>%
  setAgeMin(0)


# Prep population ---------------------------------------------------------

pop_counts <- combined_data %>%
  select(-deaths) %>%
  xtabs(population ~ age + sex + location, data = .) %>%
  Counts() %>%
  setAgeMin(0)


# Define aggregate mapping ------------------------------------------------

region_concordance <- combined_data %>%
  select(region, location) %>%
  unique() %>%
  Concordance()


# Save prepped data -------------------------------------------------------

saveRDS(death_counts, "data/prepped/death_counts.RDS")
saveRDS(pop_counts, "data/prepped/pop_counts.RDS")
saveRDS(region_concordance, "data/prepped/region_concordance.RDS")
