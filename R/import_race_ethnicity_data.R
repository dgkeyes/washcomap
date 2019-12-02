#' Title
#'
#' @param sheet_name
#'
#' @return
#' @export
#'
#' @examples
import_race_ethnicity_data <- function(sheet_name) {
  readxl::read_excel("data-raw/Health Share Counts Combined.xlsx",
                     sheet = sheet_name) %>%
    janitor::clean_names() %>%
    dplyr::select(current_school_nm, median_value) %>%
    purrr::set_names(c("school", "median_value")) %>%
    dplyr::left_join(wash_co_schools, by = "school") %>%
    dplyr::select(school, school_id, median_value) %>%
    washcomap::make_median_value_categorical()
}
