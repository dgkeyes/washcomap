
library(tidyverse)
library(janitor)
library(readxl)
library(devtools)

load_all()



snap <- read_excel("data-raw/DHS Data by Zip Code.xlsx") %>%
  clean_names() %>%
  select(zip_code, snap_children) %>%
  mutate(median = median(snap_children)) %>%
  mutate(median_value = snap_children / median) %>%
  select(-c(median, snap_children)) %>%
  make_median_value_categorical() %>%
  join_with_zcta_overlaps()



use_data(snap,
         overwrite = TRUE)

