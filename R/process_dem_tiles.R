#' Process DEM Tiles
#'
#' This function processes a list of DEM tiles, ensuring they have the same CRS and resolution.
#'
#' @param dem_files A list containing DEM file paths.
#' @param verbose Logical. If TRUE, prints detailed messages during processing.
#'
#' @return A list of processed raster tiles.
#'
#' @importFrom terra rast ext crs res project resample merge
#' @export
#'
#' @examples
#' \dontrun{
#' dem_files <- list(elevation = c("dem1.tif", "dem2.tif"))
#' processed_tiles <- process_dem_tiles(dem_files, verbose = TRUE)
#' }
process_dem_tiles <- function(dem_files, verbose = FALSE) {
  if (!is.list(dem_files) || !("elevation" %in% names(dem_files))) {
    stop("Input must be a list with an 'elevation' component")
  }

  message(sprintf("Processing %d tiles...", length(dem_files$elevation)))
  rast_list <- list()

  for (i in seq_along(dem_files$elevation)) {
    tryCatch({
      rast_list[[i]] <- terra::rast(dem_files$elevation[i])
      if(verbose) {
        message(sprintf("\nTile %d:", i))
        message("Extent: ", paste(terra::ext(rast_list[[i]]), collapse = ", "))
        crs_string <- terra::crs(rast_list[[i]])
        epsg_code <- regmatches(crs_string, regexpr("EPSG\",\\d+", crs_string))
        epsg_code <- gsub("EPSG\",", "", epsg_code)
        message("EPSG: ", epsg_code)
        message("Resolution: ", paste(terra::res(rast_list[[i]]), collapse = ", "))
      }
    }, error = function(e) {
      warning(sprintf("Failed to load tile %d: %s", i, e$message))
    })
  }

  rast_list <- Filter(Negate(is.null), rast_list)

  if (length(rast_list) == 0) {
    stop("No tiles were successfully loaded")
  }

  if (length(rast_list) == 1) {
    message("Only one tile was successfully loaded")
    return(rast_list[[1]])
  }

  ref_ext <- terra::ext(rast_list[[1]])
  ref_crs <- terra::crs(rast_list[[1]])

  if(verbose) message("\nChecking for extent and CRS mismatches...")
  for (i in seq_along(rast_list)) {
    curr_ext <- terra::ext(rast_list[[i]])
    curr_crs <- terra::crs(rast_list[[i]])

    if (!terra::same.crs(ref_crs, curr_crs)) {
      message(sprintf("CRS mismatch in tile %d", i))
      if(verbose) {
        message("Reference CRS: ", ref_crs)
        message("Tile CRS: ", curr_crs)
      }
      rast_list[[i]] <- terra::project(rast_list[[i]], ref_crs)
    }
  }

  all_res <- do.call(rbind, lapply(rast_list, terra::res))
  target_res <- apply(all_res, 2, max)
  if(verbose) message(sprintf("\nUsing coarsest resolution: %f, %f", target_res[1], target_res[2]))

  ref_rast <- rast_list[[1]]
  terra::res(ref_rast) <- target_res

  for (i in seq_along(rast_list)) {
    current_res <- terra::res(rast_list[[i]])
    if (!all(current_res == target_res)) {
      if(verbose) message(sprintf("Resampling tile %d to match target resolution", i))
      rast_list[[i]] <- terra::resample(rast_list[[i]], ref_rast,ef_rast, method = "bilinear")
    }
  }
  return(rast_list)
}
