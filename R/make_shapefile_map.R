#' Title
#'
#' @param df
#' @param tooltip_column
#' @param pre_tooltip_text
#'
#' @return
#' @export
#'
#' @examples
make_shapefile_map <- function(df, tooltip_column, pre_tooltip_text) {

  df <- df %>%
    dplyr::mutate(tooltip = base::paste0(pre_tooltip_text, {{tooltip_column}})) %>%
    sf::st_as_sf()

  mapdeck::mapdeck(style = mapdeck::mapdeck_style("light"),
                   pitch = 15,
                   location = c(-123.096819, 45.5),
                   zoom = 9) %>%
    mapdeck::add_sf(data = df,
                    tooltip = "tooltip",
                    stroke_colour = "#828282",
                    stroke_width = 50,
                    fill_colour = "#eff3ff95",
                    auto_highlight = TRUE,
                    update_view = FALSE)

}
