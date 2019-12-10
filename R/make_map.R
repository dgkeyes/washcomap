#' Title
#'
#' @param df
#' @param tooltip_col
#'
#' @return
#' @export
#'
#' @examples
make_map <- function(df) {

  # Add fill_color variable

  fill_colors <- c('#eff3ff95',
                   '#bdd7e795',
                   '#6baed695',
                   '#3182bd95',
                   '#08519c95')

  df <- df %>%
    dplyr::mutate(fill_color = dplyr::case_when(
      median_value_categorical == "Below Median" ~ fill_colors[1],
      median_value_categorical == "4th Tier" ~ fill_colors[2],
      median_value_categorical == "3rd Tier" ~ fill_colors[3],
      median_value_categorical == "2nd Tier" ~ fill_colors[4],
      median_value_categorical == "1st Tier" ~ fill_colors[5],
    )) %>%

    # Join with school catchment areas shapefile

    dplyr::left_join(wash_co_school_catchment_areas, by = "school_id") %>%
    dplyr::select(-row) %>%
    sf::st_as_sf() %>%

    # Add tooltip column

    dplyr::mutate(tooltip = stringr::str_glue("{school}<br>{district} District<br>{median_value_categorical}"))

  # Add schools as points

  wash_co_schools_geocoded <- wash_co_schools_geocoded %>%
    dplyr::mutate(tooltip = stringr::str_glue("{school} ({district})"))

  # Legend Stuff

  wash_co_legend <- mapdeck::legend_element(
    variables = c("Below Median",
                  "4th Tier",
                  "3rd Tier",
                  "2nd Tier",
                  "1st Tier"),
    colours = c(fill_colors[1],
                fill_colors[2],
                fill_colors[3],
                fill_colors[4],
                fill_colors[5]),
    colour_type = "fill",
    variable_type = "category"
  )

  wash_co_legend <- mapdeck::mapdeck_legend(wash_co_legend)

  # Make map

  mapdeck::mapdeck(style = mapdeck::mapdeck_style("light"),
                   pitch = 10,
                   location = c(-123.096819, 45.5),
                   zoom = 9) %>%
    mapdeck::add_sf(data = df,
                    tooltip = "tooltip",
                    stroke_colour = "#828282",
                    stroke_width = 50,
                    fill_colour = "fill_color",
                    auto_highlight = TRUE,
                    update_view = FALSE,
                    legend = wash_co_legend) %>%
    mapdeck::add_sf(data = wash_co_schools_geocoded,
                    radius_min_pixels = 4,
                    radius_max_pixels = 4,
                    update_view = FALSE,
                    tooltip = "tooltip",
                    fill_colour = "#828282")

}
