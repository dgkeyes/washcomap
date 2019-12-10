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
    dplyr::group_by(school_id, median_value_raw) %>%
    dplyr::summarize(median_value = mean(median_value)) %>%
    dplyr::left_join(wash_co_schools, by = "school_id") %>%
    tidyr::drop_na(school) %>%
    dplyr::select(contains("school"), contains("district"), contains("median")) %>%
    dplyr::ungroup()
}
