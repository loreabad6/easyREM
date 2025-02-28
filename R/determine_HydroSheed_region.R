#' Determine HydroSHEDS Region
#'
#' This function determines the HydroSHEDS region based on given longitude and latitude.
#'
#' @param lon Longitude coordinate.
#' @param lat Latitude coordinate.
#'
#' @return A character string indicating the region(s) that the coordinates fall into.
#'
#' @export
#'
#' @examples
#' region <- determine_hydrosheeds_region(lon = 10, lat = 50)
determine_hydrosheeds_region <- function(lon, lat) {
  regions <- list(
    africa = list(
      xmin = -17.99375, xmax = 54.41667,
      ymin = -34.79375, ymax = 37.33125
    ),
    arctic = list(
      xmin = -179.99792, xmax = -61.11875,
      ymin = 51.22708, ymax = 83.20625
    ),
    asia = list(
      xmin = 57.639583, xmax = 150.468750,
      ymin = 1.272917, ymax = 55.900000
    ),
    australasia = list(
      xmin = 95.10417, xmax = 179.99792,
      ymin = -54.73958, ymax = 20.79583
    ),
    europe = list(
      xmin = -24.47292, xmax = 69.51667,
      ymin = 12.60208, ymax = 81.78958
    ),
    greenland = list(
      xmin = -72.66458, xmax = -12.30208,
      ymin = 59.80208, ymax = 83.58958
    ),
    north_america = list(
      xmin = -137.935417, xmax = -52.664583,
      ymin = 5.510417, ymax = 62.672917
    ),
    south_america = list(
      xmin = -91.65625, xmax = -34.79375,
      ymin = -55.87708, ymax = 14.87708
    ),
    siberia = list(
      xmin = 59.01667, xmax = 179.99792,
      ymin = 45.60208, ymax = 81.18958
    )
  )

  matching_regions <- c()
  for (region_name in names(regions)) {
    region <- regions[[region_name]]
    if (lon >= region$xmin && lon <= region$xmax &&
        lat >= region$ymin && lat <= region$ymax) {
      matching_regions <- c(matching_regions, region_name)
    }
  }

  if (length(matching_regions) == 0) {
    return("No matching region found")
  } else if (length(matching_regions) == 1) {
    return(matching_regions[1])
  } else {
    return(paste("Multiple regions:", paste(matching_regions, collapse = ", ")))
  }
}