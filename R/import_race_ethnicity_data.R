#' Title
#'
#' @param sheet_name
#'
#' @return
#' @export
#'
#' @examples
import_race_ethnicity_data <- function(sheet_name) {
  readxl::read_excel(here("data-raw/Health Share Counts Combined.xlsx"),
                     sheet = sheet_name) %>%
    janitor::clean_names() %>%
    dplyr::select(current_school_nm, median_value, contains("total")) %>%
    purrr::set_names(c("school", "median_value", "total")) %>%
    dplyr::mutate(median_value_raw = median(total, na.rm = TRUE)) %>%
    dplyr::left_join(wash_co_schools, by = "school") %>%
    dplyr::select(school_id, school, district_id, district, median_value, median_value_raw)
}
