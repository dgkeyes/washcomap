
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
    # If median_value is less than 1, which means it is below the median, set median_value_categorical to 0.
    # Otherwise, set is as the value of median_value
    dplyr::mutate(median_value_categorical = dplyr::case_when(
      median_value < 1 ~ 0,
      TRUE ~ median_value
    )) %>%
    # Convert any median_value_categorical 0 values to NA so I can do quartiles with the remaining observations
    dplyr::mutate(median_value_categorical = dplyr::na_if(median_value_categorical, 0)) %>%
    # Use the remaining observations to generate quartiles
    dplyr::mutate(median_value_categorical = dplyr::ntile(median_value_categorical, 4)) %>%
    # Convert the numberic quartiles into 1st, 2nd, 3rd, 4th, and Below Median
    dplyr::mutate(median_value_categorical = dplyr::case_when(
      median_value_categorical == 1 ~ "4th Tier",
      median_value_categorical == 2 ~ "3rd Tier",
      median_value_categorical == 3 ~ "2nd Tier",
      median_value_categorical == 4 ~ "1st Tier",
      is.na(median_value_categorical) ~ "Below Median"
    ))

}

