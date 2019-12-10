#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
pull_median_value <- function(df) {
  df %>%
    dplyr::pull(median_value_raw) %>%
    base::unique()
}
