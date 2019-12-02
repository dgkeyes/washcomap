
# Packages ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(oregoneddata)
library(tidycensus)
library(devtools)


load_all()

# 100% and 200% FPL -------------------------------------------------------

poverty_variables <- load_variables(2017, "acs5", cache = TRUE) %>%
  filter(str_detect(name, "B17024")) %>%
  filter(str_detect(label, "Under 6 years")) %>%
  filter(name != "B17024_002") %>%
  select(-concept)

dk_get_100_200_poverty_data <- function(variable) {

  get_acs(state = "OR",
          county = "Washington",
          geography = "tract",
          summary_var = "B17024_002",
          variables = variable)
}

children_in_poverty <- map_df(poverty_variables$name, dk_get_100_200_poverty_data) %>%
  clean_names() %>%
  set_names(c("geoid",
              "census_tract",
              "variable",
              "children_under_6_in_poverty_n",
              "children_under_6_in_poverty_moe",
              "total_children_under_6",
              "total_children_under_moe")) %>%
  left_join(poverty_variables, by = c("variable" = "name"))

total_children_under_6 <- children_in_poverty %>%
  select(geoid, total_children_under_6) %>%
  distinct(geoid, .keep_all = TRUE)

children_in_poverty <- children_in_poverty %>%
  mutate(poverty_level = case_when(
    variable %in% c("B17024_003", "B17024_004", "B17024_005") ~ "Up to 100% FPL",
    variable %in% c("B17024_006", "B17024_007", "B17024_008", "B17024_009", "B17024_010") ~ "Between 100% and 200% FPL",
    TRUE ~ "At 200% FPL or Above"
  )) %>%
  group_by(geoid, poverty_level) %>%
  summarize(children_under_6_in_poverty_n = sum(children_under_6_in_poverty_n)) %>%
  ungroup() %>%
  left_join(total_children_under_6, by = "geoid") %>%
  mutate(children_under_6_in_poverty_pct = children_under_6_in_poverty_n / total_children_under_6) %>%
  rename("census_tract" = "geoid")




# 100% FPL ----------------------------------------------------------------

children_in_poverty_100_fpl <- children_in_poverty %>%
  filter(poverty_level == "Up to 100% FPL") %>%
  select(census_tract, children_under_6_in_poverty_pct) %>%
  mutate(median = median(children_under_6_in_poverty_pct)) %>%
  mutate(median_value = children_under_6_in_poverty_pct / median) %>%
  make_median_value_categorical() %>%
  select(census_tract, median_value, median_value_categorical) %>%
  join_with_census_tract_overlaps()

use_data(children_in_poverty_100_fpl,
         overwrite = TRUE)


# 200% FPL ----------------------------------------------------------------

children_in_poverty_200_fpl <- children_in_poverty %>%
  filter(poverty_level == "Between 100% and 200% FPL") %>%
  select(census_tract, children_under_6_in_poverty_pct) %>%
  mutate(median = median(children_under_6_in_poverty_pct)) %>%
  mutate(median_value = children_under_6_in_poverty_pct / median) %>%
  make_median_value_categorical() %>%
  select(census_tract, median_value, median_value_categorical) %>%
  join_with_census_tract_overlaps()

use_data(children_in_poverty_200_fpl,
         overwrite = TRUE)




# For Evan ----------------------------------------------------------------

children_in_poverty %>%
  pivot_wider(id_cols = census_tract,
              names_from = poverty_level,
              values_from = children_under_6_in_poverty_n) %>%
  write_csv("inst/data-for-evan/poverty-by-census-tract.csv")

children_in_poverty %>%
  mutate(median = median(children_under_6_in_poverty_pct)) %>%
  left_join(overlaps_census_tracts, by = "census_tract") %>%
  group_by(school_id, poverty_level) %>%
  summarize(children_in_poverty = mean(children_under_6_in_poverty_n)) %>%
  left_join(wash_co_schools, by = "school_id") %>%
  mutate(children_in_poverty = number(children_in_poverty, 0.1)) %>%
  select(school, district, children_in_poverty, poverty_level) %>%
  pivot_wider(id_cols = c(school_id, school),
              names_from = poverty_level,
              values_from = children_in_poverty) %>%
  ungroup() %>%
  select(-school_id) %>%
  write_csv("inst/data-for-evan/poverty-by-school.csv")

