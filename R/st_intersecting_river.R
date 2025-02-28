#' Intersect Rivers with a Bounding Box
#'
#' This function clips river segments that intersect with a given bounding box.
#'
#' @param rivers_sf An `sf` object containing river data.
#' @param bbox_polygon An `sf` or `sfc` object representing the bounding box.
#'
#' @return An `sf` object containing the clipped river segments.
#'
#' @importFrom sf st_bbox st_transform st_intersects st_intersection
#' @export
#'
#' @examples
#' \dontrun{
#' bbox <- create_bbox(xmin = -10, xmax = 10, ymin = -10, ymax = 10, crs = 4326)
#' rivers <- download_hydrorivers(region = "eu")
#' clipped_rivers <- st_intersecting_river(rivers, bbox)
#' }
st_intersecting_river <- function(rivers_sf, bbox_polygon) {
  if (!inherits(rivers_sf, "sf")) {
    stop("rivers_sf must be an sf object")
  }

  if (!inherits(bbox_polygon, "sf") && !inherits(bbox_polygon, "sfc")) {
    stop("bbox_polygon must be an sf or sfc object")
  }

  if (sf::st_crs(rivers_sf) != sf::st_crs(bbox_polygon)) {
    message("Converting CRS to match bbox_polygon...")
    rivers_sf <- sf::st_transform(rivers_sf, sf::st_crs(bbox_polygon))
  }

  rivers_bbox <- sf::st_bbox(rivers_sf)
  query_bbox <- sf::st_bbox(bbox_polygon)

  if (query_bbox["xmax"] < rivers_bbox["xmin"] ||
      query_bbox["xmin"] > rivers_bbox["xmax"] ||
      query_bbox["ymax"] < rivers_bbox["ymin"] ||
      query_bbox["ymin"] > rivers_bbox["ymax"]) {
    stop(sprintf(
      "Bbox is outside the rivers dataset extent.\n
      Rivers extent: xmin=%f, ymin=%f, xmax=%f, ymax=%f\n
      Query bbox: xmin=%f, ymin=%f, xmax=%f, ymax=%f",
      rivers_bbox["xmin"], rivers_bbox["ymin"], rivers_bbox["xmax"], rivers_bbox["ymax"],
      query_bbox["xmin"], query_bbox["ymin"], query_bbox["xmax"], query_bbox["ymax"]
    ))
  }

  if (query_bbox["xmin"] < rivers_bbox["xmin"] ||
      query_bbox["xmax"] > rivers_bbox["xmax"] ||
      query_bbox["ymin"] < rivers_bbox["ymin"] ||
      query_bbox["ymax"] > rivers_bbox["ymax"]) {
    warning(sprintf(
      "Bbox extends beyond the rivers dataset extent.\n
      Rivers extent: xmin=%f, ymin=%f, xmax=%f, ymax=%f\n
      Query bbox: xmin=%f, ymin=%f, xmax=%f, ymax=%f",
      rivers_bbox["xmin"], rivers_bbox["ymin"], rivers_bbox["xmax"], rivers_bbox["ymax"],
      query_bbox["xmin"], query_bbox["ymin"], query_bbox["xmax"], query_bbox["ymax"]
    ))
  }

  message("Finding rivers that intersect with bbox...")
  intersecting_idx <- sf::st_intersects(rivers_sf, bbox_polygon, sparse = FALSE)
  candidate_rivers <- rivers_sf[intersecting_idx, ]

  if (nrow(candidate_rivers) == 0) {
    message("No rivers found within the bbox")
    return(NULL)
  }

  message("Clipping rivers at bbox boundary...")
  clipped_rivers <- sf::st_intersection(candidate_rivers, bbox_polygon)

  message(sprintf("Found %d river segments after clipping", nrow(clipped_rivers)))

  return(clipped_rivers)
}