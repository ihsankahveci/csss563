

# Load libraries ----------------------------------------------------------

library(dembase)
library(demlife)
library(dplyr)


# Load data ---------------------------------------------------------------

pop_counts  <- readRDS("data/prepped/pop_counts.RDS")
region_conc <- readRDS("data/prepped/region_concordance.RDS")

region_ex_direct <- readRDS("data/results/region_ex_direct.rds")
model_mx         <- readRDS("data/results/model_mx_both.RDS")


# Get modeled region ex ---------------------------------------------------

region_ex_model <- model_mx %>%
  collapseCategories(
    dimension = "location",
    concordance = region_conc,
    weights = pop_counts
  ) %>%
  LifeTable() %>%
  lifeExpectancy() %>%
  collapseIterations(FUN = median) %>%
  as_tibble()


# Combine ex --------------------------------------------------------------

region_ex_both <- region_ex_direct %>%
  as_tibble() %>%
  mutate(variant = "Direct") %>%
  bind_rows(region_ex_model)


# Save data ---------------------------------------------------------------

saveRDS(region_ex_both, "data/results/region_ex_both.RDS")
