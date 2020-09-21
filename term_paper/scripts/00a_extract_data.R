
# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(pdftools)


# Define mapping table ----------------------------------------------------

age_group_tbl <- tibble(
  id = 1:21,
  age_group_name = c(
    "All ages", "0",
    "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
    "85 plus", "Unknown"
  ),
  age_group_years_start = c(0, 0, 1, seq(5, 85, 5), -1),
  age_group_year_end = c(Inf, 1, seq(5, 85, 5), Inf, -1)
)


# Define helper functions -------------------------------------------------

load_pdf_tbl <- function(tbl_text, skip_first_n, skip_last_n, colname_set) {

  tbl_lines <- tbl_text %>% read_lines(skip = skip_first_n)

  if (skip_last_n != 0) {
    tbl_lines <- head(tbl_lines, n = -1 * abs(skip_last_n))
  }

  read_table(tbl_lines, na = "-") %>%
    unite("location", starts_with("X"), sep = " ") %>%
    select(!(contains("Both") | contains("Sexes"))) %>%
    mutate(
      location = str_squish(location),
      across(-location, as.numeric)
    ) %>%
    filter(location != "")
}

update_colnames <- function(data, age_group_ids, age_map_tbl) {

  age_group_names <- age_map_tbl %>%
    filter(id %in% age_group_ids) %>%
    pull(age_group_name)

  age_sex_names <- paste(c("male", "female"), rep(age_group_names, each = 2), sep = "_")
  colnames(data) <- c("location", age_sex_names)
  data

}


# Load pdf data -----------------------------------------------------------

pdf_file <- "data/raw/2015_VSR_Vol_3_Death_Statistics.pdf"
pdf_text <- pdf_text(pdf_file)

tbl_pages <- 56:73
colname_set_factor <- 3

params <- tibble(
  tbl_text     = pdf_text[tbl_pages],
  skip_first_n = c(3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5),
  skip_last_n  = c(2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2),
  colname_set  = rep(seq(1, length(tbl_pages) / colname_set_factor, 1), each = colname_set_factor)
)

tbl_extracted <- params %>%
  pmap(load_pdf_tbl) %>%
  split(., ceiling(seq_along(.)/colname_set_factor)) %>%
  map(bind_rows) %>%
  map2(
    list(1:3, 4:7, 8:11, 12:15, 16:19, 20:21),
    ~update_colnames(data = .x, age_group_ids = .y, age_map_tbl = age_group_tbl)
  ) %>%
  reduce(left_join, by = "location")


# Format extracted data ---------------------------------------------------

tbl_formatted <- tbl_extracted %>%
  mutate(
    parent_location = if_else(location == str_to_upper(location), location, NA_character_)
  ) %>%
  fill(parent_location, .direction = "down") %>%
  mutate(
    parent_location = if_else(parent_location == location, "PHILIPPINES", parent_location),
    parent_location = if_else(location == "PHILIPPINES", "NONE", parent_location)
  ) %>%
  select(parent_location, location, everything()) %>%
  pivot_longer(
    cols = !contains("location"),
    names_to = c("sex", "age_group_name"),
    names_pattern = "(.*)_(.*)",
    values_to = "deaths"
  ) %>%
  filter(!age_group_name %in% c("All ages", "Unknown")) %>%
  left_join(age_group_tbl, by = "age_group_name") %>%
  select(parent_location, location, sex, age_group_name, age_group_years_start, deaths)


# Save data ---------------------------------------------------------------

saveRDS(tbl_formatted, "data/extracted/phl_2015_deaths.RDS")

