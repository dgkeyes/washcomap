#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
join_with_census_tract_overlaps <- function(df) {
  df %>%
    dplyr::left_join(overlaps_census_tracts, by = "census_tract") %>%
    dplyr::group_by(school_id) %>%
    dplyr::summarize(median_value = mean(median_value)) %>%
    washcomap::make_median_value_categorical() %>%
    dplyr::left_join(wash_co_schools, by = "school_id") %>%
    tidyr::drop_na(school) %>%
    dplyr::select(school_id, school, district_id, district, median_value, median_value_categorical)
}
