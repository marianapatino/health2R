#' Get VO2Max Data from Apple Health Database
#'
#' This function extracts VO2Max data for a specified time frame from the Apple Health Database,
#' either passed as a dataframe or loaded from an RDS file.
#'
#' @param health_db A dataframe or the path to an RDS file containing the health data.
#' @param start_date The start of the time frame for which to extract VO2Max data.
#' @param end_date The end of the time frame for which to extract VO2Max data.
#' @return A dataframe containing the filtered VO2Max data.
#' @references Use the Apple Health documentation for reference to the data structure.
#' @note This function assumes the health data is in the local time zone of the data source.
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   health_data <- read_rds("path/to/your/health_data.rds")
#'   vo2max_data <- get_vo2max(health_db = health_data)
#' }
#' @export
get_vo2max <- function(health_db, start_date = NULL, end_date = NULL) {
  
  # Load the data from RDS if a path is provided
  if (is.character(health_db)) {
    health_db <- readRDS(health_db)
  }
  
  # If no dates are provided, use the min/max from the dataset
  if (is.null(start_date)) {
    start_date <- min(as.Date(health_db$startDate))
  } else {
    start_date <- as.Date(start_date)
  }
  if (is.null(end_date)) {
    end_date <- max(as.Date(health_db$endDate))
  } else {
    end_date <- as.Date(end_date)
  }
  
  # Filter the data for VO2Max
  vo2max_data <- health_db %>%
    filter(type == "HKQuantityTypeIdentifierVO2Max",
           as.Date(startDate) >= start_date,
           as.Date(endDate) <= end_date) %>%
    select(startDate, value)
  
  # Optionally, you may want to convert the 'value' column to a numeric type if it is not already
  vo2max_data$value <- as.numeric(vo2max_data$value)
  
  return(vo2max_data)
}
