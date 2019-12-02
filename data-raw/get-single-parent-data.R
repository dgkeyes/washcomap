
library(tidyverse)
library(janitor)
library(readxl)
library(oregoneddata)
library(devtools)
library(tidycensus)

load_all()

single_parent_variables <- load_variables(2017, "acs5", cache = TRUE) %>%
  filter(str_detect(name, "B23008")) %>%
  filter(str_detect(label, "Under 6 years")) %>%
  filter(name != "B23008_002")

dk_get_single_parent_data <- function(variable) {

  get_acs(state = "OR",
          county = "Washington",
          geography = "tract",
          summary_var = "B23008_002",
          variables = variable)
}

single_parent <- map_df(single_parent_variables$name, dk_get_single_parent_data) %>%
  clean_names() %>%
  left_join(single_parent_variables, by = c("variable" = "name")) %>%
  mutate(family_type = case_when(
    str_detect(label, "two parents") ~ "Two parents",
    TRUE ~ "Single parent"
  )) %>%
  group_by(geoid, family_type) %>%
  summarize(n = sum(estimate),
            total = sum(summary_est)) %>%
  ungroup() %>%
  filter(family_type == "Single parent") %>%
  mutate(single_parent_pct = n / total) %>%
  select(geoid, n, total, single_parent_pct) %>%
  rename("census_tract" = "geoid") %>%
  mutate(median = median(single_parent_pct)) %>%
  mutate(median_value = single_parent_pct / median) %>%
  make_median_value_categorical() %>%
  select(census_tract, median_value, median_value_categorical) %>%
  join_with_census_tract_overlaps()


use_data(single_parent,
         overwrite = TRUE)
