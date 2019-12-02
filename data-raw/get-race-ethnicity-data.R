
# Packages ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(oregoneddata)
library(devtools)

load_all()

# Race/Ethnicity ----------------------------------------------------------

# This data comes from Adam at Washington County

race_ethnicity_composite <- read_excel("data-raw/Health Share Counts Combined.xlsx",
                                      sheet = "Composite new") %>%
  clean_names() %>%
  set_names(c("school", "median_value")) %>%
  left_join(wash_co_schools, by = "school") %>%
  select(school, school_id, median_value) %>%
  make_median_value_categorical()

use_data(race_ethnicity_composite,
         overwrite = TRUE)

race_ethnicity_black <- import_race_ethnicity_data("Black")

use_data(race_ethnicity_black,
         overwrite = TRUE)

race_ethnicity_aian <- import_race_ethnicity_data("AIAN")

use_data(race_ethnicity_aian,
         overwrite = TRUE)

race_ethnicity_asian <- import_race_ethnicity_data("Asian")

use_data(race_ethnicity_asian,
         overwrite = TRUE)

race_ethnicity_white <- import_race_ethnicity_data("Caucasian")

use_data(race_ethnicity_white,
         overwrite = TRUE)

race_ethnicity_latino <- import_race_ethnicity_data("Hispanic")

use_data(race_ethnicity_latino,
         overwrite = TRUE)

race_ethnicity_other <- import_race_ethnicity_data("Other")

use_data(race_ethnicity_other,
         overwrite = TRUE)
