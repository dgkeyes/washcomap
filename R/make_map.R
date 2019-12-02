#' Title
#'
#' @param df
#' @param tooltip_col
#'
#' @return
#' @export
#'
#' @examples
make_map <- function(df, tooltip_col) {
  mapdeck::mapdeck(style = mapdeck::mapdeck_style("light"),
                 pitch = 15,
                 location = c(-123.096819, 45.5),
                 zoom = 9) %>%
  mapdeck::add_sf(data = df,
         fill_colour = "row",
         tooltip = tooltip_col,
         fill_opacity = 150,
         stroke_width = 20,
         stroke_colour = "#ffffff",
         auto_highlight = TRUE,
         update_view = FALSE,
         palette = "magma",
         highlight_colour = "#ffffff99",
         na_colour = "#fafafa",
         legend = list(fill_colour = TRUE),
         legend_options = list(title = "Title"))
}
