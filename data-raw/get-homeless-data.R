
library(tidyverse)
library(janitor)
library(readxl)
library(devtools)

load_all()

homeless <- read_excel("data-raw/PreK Homeless by Zip.xlsx") %>%
  clean_names() %>%
  select(zip:median_value) %>%
  make_median_value_categorical() %>%
  rename("zip_code" = "zip") %>%
  join_with_zcta_overlaps()

use_data(homeless,
         overwrite = TRUE)
