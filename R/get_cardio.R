#' Get Cardiovascular Data from Apple Health Database
#'
#' This function extracts heart rate data for a specified type and time frame from
#' the Apple Health Database, either passed as a dataframe or loaded from an RDS file.
#'
#' @param health_db A dataframe or the path to an RDS file containing the health data.
#' @param type The type of heart rate data to extract, with default being "HKQuantityTypeIdentifierHeartRate".
#' @param var A short label for common heart rate data types as an alternative to the 'type' parameter.
#' @param start_date The start of the time frame for which to extract data.
#' @param end_date The end of the time frame for which to extract data.
#' @return A dataframe containing the filtered heart rate data.
#' @references Use the Apple Health documentation for reference to the data structure.
#' @note Available types are: "heartrate", "resting", "walkingAverage", "workout", "variability", "recovery"
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   health_data <- read_rds("path/to/your/health_data.rds")
#'   cardio_data <- get_cardio(health_db = health_data, var = "heartrate")
#' }
#' @export
get_cardio <- function(health_db, 
                       type = "HKQuantityTypeIdentifierHeartRate",
                       var = NULL,
                       start_date = NULL, 
                       end_date = NULL) {
  
  # Define a mapping of short labels to Apple Health types
  type_mapping <- c(heartrate = "HKQuantityTypeIdentifierHeartRate",
                    resting = "HKQuantityTypeIdentifierRestingHeartRate",
                    walkingAverage = "HKQuantityTypeIdentifierWalkingHeartRateAverage",
                    workout = "HKQuantityTypeIdentifierHeartRateDuringWorkout",
                    variability = "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
                    recovery = "HKQuantityTypeIdentifierHeartRateRecovery")
  
  # If var is provided, use it to set the type
  if (!is.null(var)) {
    type <- type_mapping[[var]]
  }
  
  
  # Load the data from RDS if a path is provided
  if (is.character(health_db)) {
    health_db <- readRDS(health_db)
  }
  
  # If no dates are provided, use the min/max from the dataset
  if (is.null(start_date)) {
    start_date <- min(as.POSIXct(health_db$startDate))
  } else {
    start_date <- as.POSIXct(start_date)
  }
  if (is.null(end_date)) {
    end_date <- max(as.POSIXct(health_db$endDate))
  } else {
    end_date <- as.POSIXct(end_date)
  }
  
  # Filter the data for the specified type and time frame
  cardio_data <- health_db %>%
    filter(type == !!type,
           as.POSIXct(startDate) >= start_date,
           as.POSIXct(endDate) <= end_date) %>%
    mutate(value = as.numeric(value))
  
  return(cardio_data)
}


