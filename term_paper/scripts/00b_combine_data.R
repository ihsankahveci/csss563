
# Load libraries ----------------------------------------------------------

library(dplyr)
library(tibble)
library(tidyr)
library(stringr)


# Load data ---------------------------------------------------------------

pop_data <- readRDS("data/extracted/phl_2015_pop.RDS")
death_data <- readRDS("data/extracted/phl_2015_deaths.RDS")


# Collapse to 80+ terminal age --------------------------------------------

deaths_80plus <- death_data %>%
  filter(age_group_years_start >= 80) %>%
  group_by(parent_location, location, sex) %>%
  summarise(
    age_group_name = "80 plus",
    age_group_years_start = 80,
    deaths = sum(deaths, na.rm = TRUE)
  )

death_data_80plus <- death_data %>%
  filter(age_group_years_start < 80) %>%
  bind_rows(deaths_80plus)


# Reconcile locations -----------------------------------------------------

death_data_prep <- death_data_80plus %>%
  filter(
    parent_location != "NATIONAL CAPITAL REGION",
    !location %in% c("NATIONAL CAPITAL REGION", "FOREIGN COUNTRIES"),
    !str_detect(location, "City")
  ) %>%
  mutate(location = case_when(
    location == "PHILIPPINES" ~ "Philippines",
    location == "Samar" ~ "Samar (Western Samar)",
    location == "Cotabato" ~ "Cotabato (North Cotabato)",
    location == "Davao" ~ "Davao Del Norte",
    TRUE ~ location
  ))

pop_data_prep <- pop_data %>%
  filter(location != "National Capital Region")

# Test that that all non-region locations match
loc_match_tbl <- full_join(
  tibble(pop_loc = unique(pop_data_prep$location)) %>%
    mutate(location_name = str_to_title(pop_loc)),
  tibble(death_loc = unique(death_data_prep$location)) %>%
    mutate(location_name = str_to_title(death_loc)),
  by = "location_name"
) %>%
  mutate(is_same = pop_loc == death_loc)

loc_diffs <- loc_match_tbl %>%
  filter((is.na(pop_loc) | is.na(death_loc))) %>%
  filter(!str_detect(location_name, "Region"))


# Combine data ------------------------------------------------------------

all_data_prep <- death_data_prep %>%
  left_join(
    pop_data_prep,
    by = c("location", "sex", "age_group_name", "age_group_years_start")
  )

# Compare aggregates
total_region_deaths <- all_data_prep %>%
  filter(str_detect(location, "REGION")) %>%
  group_by(location) %>%
  summarise(direct_deaths = sum(deaths, na.rm = TRUE)) %>%
  rename(region = location)

total_region_deaths_agg <- all_data_prep %>%
  group_by(parent_location) %>%
  summarise(agg_deaths = sum(deaths, na.rm = TRUE)) %>%
  rename(region = parent_location)

compare_deaths_tbl <- total_region_deaths_agg %>%
  left_join(total_region_deaths, by = "region") %>%
  mutate(diff = agg_deaths - direct_deaths)


# Save combined data ------------------------------------------------------

all_data_final <- all_data_prep %>%
  filter(!parent_location %in% c("PHILIPPINES", "NONE")) %>%
  rename(
    region = parent_location,
    province = location
  )

saveRDS(all_data_final, "data/combined/phl_2015_all.RDS")
