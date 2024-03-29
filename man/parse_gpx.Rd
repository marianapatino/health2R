% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_workouts.R
\name{parse_gpx}
\alias{parse_gpx}
\title{Internal Function to Parse GPX Files}
\usage{
parse_gpx(file_path)
}
\arguments{
\item{file_path}{The path to the GPX file to be parsed.}
}
\value{
A list containing the route name and a tibble of the GPX data.
}
\description{
This function reads a GPX file and extracts GPS track information
including latitude, longitude, elevation, time, speed, course,
horizontal accuracy, and vertical accuracy.
}
\examples{
\dontrun{
gpx_data <- parse_gpx("path/to/your/file.gpx")
route_name <- gpx_data$route_name
track_points <- gpx_data$gpx_data
}
}
