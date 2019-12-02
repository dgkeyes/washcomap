
library(tidyverse)
library(janitor)
library(readxl)
library(devtools)

load_all()

disability <- read_excel("data-raw/Disability by Elementary.xlsx") %>%
  clean_names() %>%
  left_join(wash_co_schools, by = c("school_name" = "school")) %>%
  select(school_name, school_id, median_value) %>%
  rename("school" = "school_name") %>%
  make_median_value_categorical()

use_data(disability,
         overwrite = TRUE)
