#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
join_with_school_catchment <- function(df) {
  df %>%
    dplyr::left_join(wash_co_school_catchment_areas, by = "school_id") %>%
    dplyr::select(-row) %>%
    sf::st_as_sf()
}
