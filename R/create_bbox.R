#' Create a Bounding Box (bbox) Polygon
#'
#' This function creates a bounding box polygon based on the given minimum and maximum coordinates.
#'
#' @param xmin Minimum x-coordinate (longitude).
#' @param xmax Maximum x-coordinate (longitude).
#' @param ymin Minimum y-coordinate (latitude).
#' @param ymax Maximum y-coordinate (latitude).
#' @param crs Coordinate Reference System (CRS) to be assigned to the bounding box.
#'
#' @return An `sfc` object representing the bounding box polygon.
#'
#' @importFrom sf st_sfc st_polygon
#' @export
#'
#' @examples
#' Create a bounding box
#' bbox <- create_bbox(xmin = -180, xmax = 180, ymin = -90, ymax = 90, crs = 4326)
#' bbox
create_bbox <- function(xmin, xmax, ymin, ymax, crs) {
  bbox <- sf::st_sfc(
                     sf::st_polygon(
                                   list(cbind(
                                              c(xmin, xmax, xmax, xmin, xmin),
                                              c(ymin, ymin, ymax, ymax, ymin)
                                              ))
                                    ),
                     crs=crs
                       )
  return(bbox)
 }
