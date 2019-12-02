#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
join_with_zcta_overlaps <- function(df) {
  df %>%
    dplyr::mutate(zip_code = as.character(zip_code)) %>%
    dplyr::left_join(overlaps_zcta) %>%
    dplyr::group_by(school_id) %>%
    dplyr::summarize(median_value = mean(median_value)) %>%
    washcomap::make_median_value_categorical() %>%
    dplyr::left_join(wash_co_schools) %>%
    tidyr::drop_na(school) %>%
    dplyr::select(school_id, school, district_id, district, median_value, median_value_categorical)
}
