#' Internal Function to Parse GPX Files
#'
#' This function reads a GPX file and extracts GPS track information
#' including latitude, longitude, elevation, time, speed, course,
#' horizontal accuracy, and vertical accuracy.
#'
#' @param file_path The path to the GPX file to be parsed.
#' @return A list containing the route name and a tibble of the GPX data.
#' @examples
#' \dontrun{
#' gpx_data <- parse_gpx("path/to/your/file.gpx")
#' route_name <- gpx_data$route_name
#' track_points <- gpx_data$gpx_data
#' }
#' @export
#' @export
parse_gpx <- function(file_path) {
  # Read the file
  gpx_data <- xml2::read_xml(file_path)
  
  # Define a prefix for the default namespace to use in our XPath expressions
  ns <- xml2::xml_ns_rename(xml2::xml_ns(gpx_data), d1 = "def")
  
  # Extract the route name
  route_name <- xml2::xml_text(xml2::xml_find_first(gpx_data, ".//def:trk/def:name", ns))
  
  # Find the track points, considering namespaces
  trkpts <- xml2::xml_find_all(gpx_data, ".//def:trkpt", ns)
  
  # Extract the data from each track point
  data <- purrr::map_df(trkpts, ~{
    lat <- as.numeric(xml2::xml_attr(.x, "lat"))
    lon <- as.numeric(xml2::xml_attr(.x, "lon"))
    ele <- as.numeric(xml2::xml_text(xml2::xml_find_first(.x, ".//def:ele", ns)))
    time <- as.POSIXct(xml2::xml_text(xml2::xml_find_first(.x, ".//def:time", ns)), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    extensions <- xml2::xml_find_first(.x, ".//def:extensions", ns)
    speed <- as.numeric(xml2::xml_text(xml2::xml_find_first(extensions, ".//def:speed", ns)))
    course <- as.numeric(xml2::xml_text(xml2::xml_find_first(extensions, ".//def:course", ns)))
    hAcc <- as.numeric(xml2::xml_text(xml2::xml_find_first(extensions, ".//def:hAcc", ns)))
    vAcc <- as.numeric(xml2::xml_text(xml2::xml_find_first(extensions, ".//def:vAcc", ns)))
    
    tibble::tibble(lat, lon, ele, time, speed, course, hAcc, vAcc)
  })
  
  list(route_name = route_name, gpx_data = data)
}

#' Import Workout Data from Apple Health Export
#'
#' This function processes Apple Health Export data from a specified zip file. 
#' It extracts GPS data from GPX files within the zip archive, parses the GPX 
#' data, and compiles it into a named list of tibbles where each tibble 
#' represents a workout session.
#'
#' @param filename The path to the zip file containing Apple Health Export data.
#' @param datasource The source of the health data, default is set to 'apple'.
#' @return A named list of tibbles where each tibble contains columns for 
#' latitude (lat), longitude (lon), elevation (ele), time, speed, course, 
#' horizontal accuracy (hAcc), and vertical accuracy (vAcc). The names of the 
#' list elements correspond to the names of the workout routes.
#' @examples
#' # Assuming 'workouts.zip' is the Apple Health Export zip file in the working directory:
#' workout_data <- parse_workouts(filename = "workouts.zip")
#' @export
import_workouts <- function(filename, 
                           datasource = "apple"){
  
  
  zip_file = filename
  # Get the temp directory
  tmpdir <- tempdir()
  # List all files and remove them from tmpdir
  tmpfiles <- list.files(path=tmpdir)
  lapply(file.path(tmpdir, tmpfiles), unlink, recursive=TRUE)
  
  # Unzip the file to temp directory and read the xml file
  unzip(zip_file, exdir = tmpdir)
  
  # list contents of temp directory
  list.files(tmpdir)
  
  # find the xml file (should not contain "_cda")
  gps_files <- list.files(tmpdir, 
                          pattern = ".gpx", 
                          full.names = TRUE,recursive = T) #%>% rev %>% .[1:25]
  print(gps_files)
  #####################
  pb <- dplyr::progress_estimated(length(gps_files))
  
    records_data <- 
      purrr::map(gps_files, 
                     ~{
                       pb$tick()$print()
                       gpxfile <- .
                       out <- parse_gpx(file_path = gpxfile)
                       return(out)
                     })
    
    workouts <- 
      purrr::map(records_data,
          ~ .x$gpx_data) %>% purrr::set_names(purrr::map(records_data,~.x$route_name))
    

  return(workouts)
}

# saveRDS(workouts,"dev/data/parsed_workouts_mp.rds")

