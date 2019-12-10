#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
make_table <- function(df) {

  df %>%
    dplyr::ungroup() %>%
    dplyr::left_join(child_care_providers_erdc, by = "school_id") %>%
    dplyr::left_join(head_start_providers, by = "school_id") %>%
    dplyr::select(school.x,
                  district.x,
                  median_value,
                  median_value_categorical,
                  number_of_providers.x,
                  number_of_providers.y) %>%
    dplyr::mutate(median_value = scales::number(median_value, 0.01)) %>%
    dplyr::mutate(number_of_providers.x = scales::number(number_of_providers.x, 0.1)) %>%
    dplyr::mutate(number_of_providers.y = scales::number(number_of_providers.y, 0.1)) %>%
    dplyr::arrange(school.x) %>%
    purrr::set_names(c("School",
                       "District",
                       "Score Relative to Median",
                       "Tier",
                       "ERDC Providers",
                       "Head Start Providers")) %>%
    reactable::reactable(searchable = TRUE,
                         highlight = TRUE)

}

