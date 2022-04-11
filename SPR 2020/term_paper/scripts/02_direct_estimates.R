

# Load libraries ----------------------------------------------------------

library(dembase)
library(demlife)


# Load data ---------------------------------------------------------------

death_counts       <- readRDS("data/prepped/death_counts.RDS")
pop_counts         <- readRDS("data/prepped/pop_counts.RDS")
region_concordance <- readRDS("data/prepped/region_concordance.RDS")

region_deaths <- collapseCategories(
  death_counts,
  dimension = "location",
  concordance = region_concordance
)

region_pop <- collapseCategories(
  pop_counts,
  dimension = "location",
  concordance = region_concordance
)


# Calculate direct estimates ----------------------------------------------

# Province level
province_mx <- death_counts / pop_counts
province_life_table <- LifeTable(province_mx)
province_ex <- lifeExpectancy(province_life_table)

# Region level
region_mx <- region_deaths / region_pop
region_life_table <- LifeTable(region_mx)
region_ex <- lifeExpectancy(region_life_table)


# Save direct estimates ---------------------------------------------------

saveRDS(region_ex, "data/results/region_ex_direct.rds")
