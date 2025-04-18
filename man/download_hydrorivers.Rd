% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_HydroSheed_Rivers.R
\name{download_hydrorivers}
\alias{download_hydrorivers}
\title{Download HydroRIVERS Data}
\usage{
download_hydrorivers(region = "global", base_dir = getwd())
}
\arguments{
\item{region}{Character string specifying the region. Valid options are "global", "af", "ar", "as", "au", "eu", "na", "sa".}

\item{base_dir}{Directory where the data will be downloaded. Defaults to the current working directory.}
}
\value{
An `sf` object containing the river data.
}
\description{
This function downloads HydroRIVERS data for a specified region from the HydroSHEDS database.
}
\examples{
\dontrun{
rivers <- download_hydrorivers(region = "eu")
}
}
