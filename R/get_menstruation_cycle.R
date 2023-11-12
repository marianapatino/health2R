#' Get Menstruation Cycle Data from Apple Health Database
#'
#' This function identifies menstruation periods from the Apple Health Database, either passed as a dataframe
#' or loaded from an RDS file, and returns a dataframe with a complete sequence of dates and a flag
#' indicating menstruation days within the range where menstruation data exists.
#'
#' @param health_db A dataframe or the path to an RDS file containing the health data.
#' @param type The type of menstruation data to extract, with default being "HKCategoryTypeIdentifierMenstrualFlow".
#' @param start_date The start of the time frame for which to extract data.
#' @param end_date The end of the time frame for which to extract data.
#' @return A dataframe with a sequence of dates and a flag for menstruation days.
#' @references Use the Apple Health documentation for reference to the data structure.
#' @note This function assumes the health data is in UTC time zone and that menstruation data includes 'startDate' and 'endDate'.
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   health_data <- read_rds("path/to/your/health_data.rds")
#'   menstruation_cycle_data <- get_menstruation_cycle(health_db = health_data)
#' }
#' @export
get_menstruation_cycle <- function(health_db, 
                                   type = "HKCategoryTypeIdentifierMenstrualFlow",
                                   start_date = NULL, 
                                   end_date = NULL) {
  
  # Load the data from RDS if a path is provided
  if (is.character(health_db)) {
    health_db <- readRDS(health_db)
  }
  
  # Filter for menstruation data
  menstruation_data <- health_db %>%
    filter(type == !!type) %>%
    select(startDate, endDate) %>%
    distinct() %>%
    mutate(startDate = as.Date(startDate),
           endDate = as.Date(endDate))
  
  # Determine the range of dates where menstruation data exists
  if (is.null(start_date)) {
    start_date <- min(menstruation_data$startDate, na.rm = TRUE)
  } else {
    start_date <- as.Date(start_date)
  }
  if (is.null(end_date)) {
    end_date <- max(menstruation_data$endDate, na.rm = TRUE)
  } else {
    end_date <- as.Date(end_date)
  }
  
  # Create a full sequence of dates between the min and max menstruation dates
  all_dates <- tibble(date = seq(start_date, end_date, by = "day"))
  
  # Create a sequence of dates for each menstruation period and mark them
  menstruation_days <- menstruation_data %>%
    rowwise() %>%
    do(data.frame(date = seq(.$startDate, .$endDate, by = "day"))) %>%
    ungroup() %>%
    mutate(menstruation = TRUE)
  
  # Join with all_dates to mark menstruation days
  complete_menstruation_data <- all_dates %>%
    left_join(menstruation_days, by = "date") %>%
    mutate(menstruation = ifelse(is.na(menstruation), FALSE, TRUE))
  
  return(complete_menstruation_data)
}
