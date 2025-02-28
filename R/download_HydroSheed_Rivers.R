#' Download HydroRIVERS Data
#'
#' This function downloads HydroRIVERS data for a specified region from the HydroSHEDS database.
#'
#' @param region Character string specifying the region. Valid options are "global", "af", "ar", "as", "au", "eu", "na", "sa".
#' @param base_dir Directory where the data will be downloaded. Defaults to the current working directory.
#'
#' @return An `sf` object containing the river data.
#'
#' @importFrom sf st_read
#' @importFrom utils download.file unzip
#' @export
#'
#' @examples
#' \dontrun{
#' rivers <- download_hydrorivers(region = "eu")
#' }
download_hydrorivers <- function(region = "global", base_dir = getwd()) {
  valid_regions <- c("global", "af", "ar", "as", "au", "eu", "na", "sa")
  region <- tolower(region)
  if (!region %in% valid_regions) {
    stop(sprintf("Invalid region. Must be one of: %s",
                 paste(valid_regions, collapse = ", ")))
  }

  old_dir <- setwd(base_dir)
  on.exit(setwd(old_dir))

  get_rivers <- function(region) {
    if (region == "global") {
      zip_name <- "global-rivers.zip"
      dir_name <- "HydroRIVERS_v10_shp"
      urls <- c(
        "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_shp.zip",
        "https://hydrosheds.org/files/HydroRIVERS_v10_shp.zip",
        "https://hydro.nationalmap.gov/download/HydroRIVERS/HydroRIVERS_v10_shp.zip"
      )
    } else {
      zip_name <- sprintf("%s-rivers.zip", region)
      dir_name <- sprintf("HydroRIVERS_v10_%s_shp", tolower(region))
      urls <- c(
        sprintf("https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_%s_shp.zip", tolower(region)),
        sprintf("https://hydrosheds.org/files/HydroRIVERS_v10_%s_shp.zip", tolower(region)),
        sprintf("https://hydro.nationalmap.gov/download/HydroRIVERS/HydroRIVERS_v10_%s_shp.zip", tolower(region))
      )
    }

    if (!file.exists(zip_name)) {
      download_successful <- FALSE

      for (url in urls) {
        message(sprintf("Attempting download from %s", url))

        tryCatch({
          options(timeout = 300)
          methods <- c("auto", "wget", "curl", "libcurl")
          for (method in methods) {
            tryCatch({
              download.file(
                url = url,
                destfile = zip_name,
                mode = "wb",
                method = method,
                quiet = FALSE
              )
              download_successful <- TRUE
              break
            }, error = function(e) {
              message(sprintf("Method %s failed: %s", method, e$message))
            })
            if (download_successful) break
          }

          if (download_successful) break
        }, error = function(e) {
          message(sprintf("Failed to download from %s: %s", url, e$message))
        })
      }

      if (!download_successful) {
        stop("Failed to download from all available URLs. Please check your internet connection or try manually downloading from https://www.hydrosheds.org/downloads")
      }
    } else {
      message(sprintf("File %s already exists, skipping download", zip_name))
    }

    if (!file.exists(zip_name) || file.size(zip_name) < 1000) {
      file.remove(zip_name)
      stop("Downloaded file is invalid or corrupted")
    }

    if (!dir.exists(dir_name)) {
      tryCatch({
        unzip(zip_name)
        message(sprintf("Extracted %s", zip_name))
      }, error = function(e) {
        stop(sprintf("Failed to extract %s: %s", zip_name, e$message))
      })
    }

    return(dir_name)
  }

  load_rivers <- function(dir_name) {
    tryCatch({
      filenames <- list.files(
        path = dir_name,
        pattern = "\\.shp$",
        full.names = TRUE,
        recursive = TRUE
      )

      if (length(filenames) == 0) {
        stop(sprintf("No shapefile found in directory: %s", dir_name))
      }

      message(sprintf("Loading shapefile from %s", filenames[1]))
      rivers <- sf::st_read(filenames[1], quiet = TRUE)

      return(rivers)
    }, error = function(e) {
      stop(sprintf("Error loading shapefile: %s", e$message))
    })
  }

  tryCatch({
    dir_name <- get_rivers(region)
    rivers <- load_rivers(dir_name)
    return(rivers)
  }, error = function(e) {
    stop(sprintf("Error in download_hydrorivers: %s", e$message))
  })
}