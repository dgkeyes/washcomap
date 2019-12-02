
# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(tidycensus)
library(janitor)
library(DT)
library(scales)
library(readxl)
library(tigris)
library(oregoneddata)
library(zip)
library(devtools)
library(ggmap)


# School and District Data ------------------------------------------------

# These are the school districts they are using. This comes from email correspondence with Evan.

wash_co_districts <- c("Banks",
                       "Beaverton",
                       "Forest Grove",
                       "Gaston",
                       "Hillsboro",
                       "Sherwood",
                       "Tigard-Tualatin")

use_data(wash_co_districts,
         overwrite = TRUE)


# I'm getting this data so that I can do merges later on. It comes from my oregoneddata package.

wash_co_schools <- ored_enrollment_by_school %>%
  separate(district,
           into = c("district", "to_drop"),
           sep = " SD") %>%
  select(-c(to_drop, enrollment)) %>%
  mutate(school = case_when(
    school == "Groner K-8" ~ "Groner Elementary School",
    TRUE ~ school
  )) %>%
  filter(district %in% wash_co_districts)


use_data(wash_co_schools,
         overwrite = TRUE)


# Getting this from data that Evan shared. I then merge the wash_co_schools data frame from above to add things like school and district id variables

wash_co_schools_list <- c("McKinley Elementary School", "Aloha-Huber Park School", "Barnes Elementary School", "Kinnaman Elementary School", "Elmonica Elementary School", "Beaver Acres Elementary School", "Lenox Elementary School", "Cornelius Elementary School", "Metzger Elementary School", "Vose Elementary School", "Fir Grove Elementary School", "Lincoln Street Elementary School", "Hazeldale Elementary School", "Charles F Tigard Elementary School", "Chehalem Elementary School", "Witch Hazel Elementary School", "Nancy Ryles Elementary School", "Durham Elementary School", "Echo Shaw Elementary School", "William Walker Elementary School", "James Templeton Elementary School", "Joseph Gale Elementary School", "McKay Elementary School", "W Verne McKinney Elementary School", "West Union Elementary School", "Greenway Elementary School", "Mooberry Elementary School", "Reedville Elementary School", "Eastwood Elementary School", "Minter Bridge Elementary School", "Deer Creek Elementary School", "Raleigh Park Elementary School", "W L Henry Elementary School", "Errol Hassell Elementary School", "Tualatin Elementary School", "Tobias Elementary School", "Fern Hill Elementary School", "Bridgeport Elementary School", "Rosedale Elementary School", "Cooper Mountain Elementary School", "Springville K-8 School", "Raleigh Hills Elementary School", "Harvey Clarke Elementary School", "Ridgewood Elementary School", "Alberta Rider Elementary School", "Mary Woodward Elementary School", "North Plains Elementary School", "Oak Hills Elementary School", "Sexton Mountain Elementary School", "Butternut Creek Elementary School", "Imlay Elementary School", "Orenco Elementary School", "Brookwood Elementary School", "Quatama Elementary School", "Hiteon Elementary School", "Free Orchards Elementary School", "Farmington View Elementary School", "Cedar Mill Elementary School", "Jackson Elementary School", "Scholls Heights Elementary School", "Middleton Elementary School", "Indian Hills Elementary School", "Edward Byrom Elementary School", "Montclair Elementary School", "Edy Ridge Elementary School", "Banks Elementary School", "Groner Elementary School", "Dilley Elementary School", "J Clyde Hopkins Elementary School", "Ladd Acres Elementary School", "Bethany Elementary School", "Terra Linda Elementary School", "Rock Creek Elementary School", "Findley Elementary", "Paul L Patterson Elementary School", "Gaston Elementary School", "Bonny Slope Elementary School", "Jacob Wismer Elementary School", "West Tualatin View Elementary School") %>%
  tibble() %>%
  set_names("school") %>%
  left_join(wash_co_schools, by = "school")

use_data(wash_co_schools_list,
         overwrite = TRUE)



# School Catchment Shapefiles ----------------------------------------------

# Data downloaded from https://spatialdata.oregonexplorer.info/geoportal/details;id=1270fe6e833f4d0eabacc71300069738

wash_co_school_catchment_areas <- st_read("data-raw/SchoolYear_2015_2016.gdb",
                                          layer = "EDUCATIONAL_BOUNDARIES") %>%
  clean_names() %>%
  filter(grade_k_id %in% wash_co_schools$school_id) %>%
  separate(school_district_name_1,
           into = c("district", "to_drop"),
           sep = " SD") %>%
  mutate(district = str_to_title(district)) %>%
  select(district, school_district_institution_id, grade_k_name, grade_k_id) %>%
  rename("district_id" = "school_district_institution_id",
         "school" = "grade_k_name",
         "school_id" = "grade_k_id") %>%
  # Dissolved boundaries using https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/
  group_by(school_id) %>%
  summarize() %>%
  ungroup() %>%
  mutate(row = row_number()) %>%
  select(row, school_id, Shape) %>%
  rename("geometry" = "Shape") %>%
  st_as_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 4326)

use_data(wash_co_school_catchment_areas,
         overwrite = TRUE)


# ZCTA Shapefiles ---------------------------------------------------------

# Washington County list of zips from http://www.ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?FIPS=41067

wash_co_zips <- tibble::tribble(
  ~Zip.Code,          ~City,             ~County,
  97005,    "Beaverton", "Washington County",
  97006,    "Beaverton", "Washington County",
  97006,        "Aloha", "Washington County",
  97007,    "Beaverton", "Washington County",
  97007,        "Aloha", "Washington County",
  97008,    "Beaverton", "Washington County",
  97062,     "Tualatin", "Washington County",
  97075,    "Beaverton", "Washington County",
  97076,    "Beaverton", "Washington County",
  97106,        "Banks", "Washington County",
  97109,        "Banks", "Washington County",
  97109,       "Buxton", "Washington County",
  97113,    "Cornelius", "Washington County",
  97116,     "Glenwood", "Washington County",
  97116, "Forest Grove", "Washington County",
  97117,  "Gales Creek", "Washington County",
  97119,       "Gaston", "Washington County",
  97123,    "Hillsboro", "Washington County",
  97124,    "Hillsboro", "Washington County",
  97125,        "Banks", "Washington County",
  97125,      "Manning", "Washington County",
  97133, "North Plains", "Washington County",
  97140,     "Sherwood", "Washington County",
  97144,       "Timber", "Washington County",
  97223,     "Portland", "Washington County",
  97223,       "Tigard", "Washington County",
  97224,     "Portland", "Washington County",
  97224,       "Tigard", "Washington County",
  97225,     "Portland", "Washington County",
  97229,     "Portland", "Washington County",
  97281,     "Portland", "Washington County",
  97281,       "Tigard", "Washington County",
  97291,     "Portland", "Washington County",
  97298,     "Portland", "Washington County"
) %>%
  pull(Zip.Code) %>%
  c(97078, 97003, 97056, 97064, 97231, 97132) %>%
  unique()

wash_co_zctas <- zctas() %>%
  clean_names() %>%
  filter(zcta5ce10 %in% wash_co_zips) %>%
  mutate(row = row_number()) %>%
  select(row, zcta5ce10, geometry) %>%
  rename("zip_code" = "zcta5ce10") %>%
  st_transform(crs = 4326)

use_data(wash_co_zctas,
         overwrite = TRUE)



# Census Tracts -----------------------------------------------------------

wash_co_census_tracts <- tracts(state = "OR",
                                county = "Washington") %>%
  clean_names() %>%
  mutate(row = row_number()) %>%
  select(row, geoid, geometry) %>%
  rename("census_tract" = "geoid") %>%
  st_transform(crs = 4326)

use_data(wash_co_census_tracts,
         overwrite = TRUE)


# Overlaps ----------------------------------------------------------------

overlaps_zcta <- st_intersects(wash_co_school_catchment_areas,
                               wash_co_zctas) %>%
  as_tibble() %>%
  set_names(c("school_catchment_row_id",
              "zcta_row_id")) %>%
  left_join(wash_co_school_catchment_areas, by = c("school_catchment_row_id" = "row")) %>%
  select(-geometry) %>%
  left_join(wash_co_zctas, by = c("zcta_row_id" = "row")) %>%
  select(-geometry) %>%
  left_join(wash_co_schools, by = "school_id") %>%
  select(school_id, school, zip_code)

use_data(overlaps_zcta,
         overwrite = TRUE)

overlaps_census_tracts <- st_intersects(wash_co_school_catchment_areas,
                                        wash_co_census_tracts) %>%
  as_tibble() %>%
  set_names(c("school_catchment_row_id",
              "census_tract_row_id")) %>%
  left_join(wash_co_school_catchment_areas, by = c("school_catchment_row_id" = "row")) %>%
  select(-geometry) %>%
  left_join(wash_co_census_tracts, by = c("census_tract_row_id" = "row")) %>%
  select(-geometry) %>%
  left_join(wash_co_schools, by = "school_id") %>%
  select(school_id, school, census_tract)

use_data(overlaps_census_tracts,
         overwrite = TRUE)


# Schools -----------------------------------------------------------------

# Data downloaded from https://www.ode.state.or.us/instID/Default/Index

# wash_co_schools_geocoded <- read_csv("data-raw/Institution_Search_Results.csv") %>%
#   clean_names() %>%
#   filter(institution_id %in% wash_co_schools_list$school_id) %>%
#   distinct(institution_id, .keep_all = TRUE) %>%
#   left_join(wash_co_schools_list, by = c("institution_id" = "school_id")) %>%
#   select(institution_id, school:district, street_str_addr1:street_zip) %>%
#   rename("school_id" = "institution_id") %>%
#   mutate(address = str_glue("{street_str_addr1} {street_city}, {street_state} {street_zip}")) %>%
#   select(-c(street_str_addr1:street_zip)) %>%
#   mutate_geocode(address) %>%
#   st_as_sf(coords = c("lon", "lat")) %>%
#   st_set_crs(4326)
#
# use_data(wash_co_schools_geocoded,
#          overwrite = TRUE)

# Child Care Providers ----------------------------------------------------


