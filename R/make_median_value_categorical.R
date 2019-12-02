
#' Test
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
make_median_value_categorical <- function(df) {

  df %>%
    dplyr::mutate(median_value_categorical = dplyr::case_when(
      median_value < 1 ~ 0,
      TRUE ~ median_value
    )) %>%
    dplyr::mutate(median_value_categorical = dplyr::na_if(median_value_categorical, 0)) %>%
    dplyr:: mutate(median_value_categorical = dplyr::ntile(median_value_categorical, 4)) %>%
    dplyr::mutate(median_value_categorical = dplyr::case_when(
      median_value_categorical == 1 ~ "1st Quartile",
      median_value_categorical == 2 ~ "2nd Quartile",
      median_value_categorical == 3 ~ "3rd Quartile",
      median_value_categorical == 4 ~ "4th Quartile",
      is.na(median_value_categorical) ~ "Below Median"
    )) %>%
    dplyr::mutate(median_value_categorical = factor(median_value_categorical,
                                             levels = c("Below Median",
                                                        "1st Quartile",
                                                        "2nd Quartile",
                                                        "3rd Quartile",
                                                        "4th Quartile")))
}
