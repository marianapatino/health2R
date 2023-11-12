#' Add Differential Privacy to Timestamps by Column Type
#'
#' This function detects timestamp columns in a dataframe based on their type and adds a random time offset to them
#' to provide differential privacy. The offset preserves the relationship between timestamps but obscures the actual times.
#'
#' @param data A dataframe containing the data with potential timestamp columns.
#' @param max_offset_seconds The maximum number of seconds for the random time offset. Default is 86400 (24 hours).
#' @return A dataframe with the timestamp columns adjusted for differential privacy.
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   # Assuming your dataframe is named 'df'
#'   private_data <- add_privacy_to_timestamps_autotype(df, max_offset_seconds = 3600) # 1-hour max offset
#' }
#' @export
add_differential_privacy <- function(data,
                                     max_differential_privacy_offset = duration(1,"days")) {
  
  max_offset_seconds = max_differential_privacy_offset %>% lubridate::seconds()

  # Identify the timestamp columns by type
  timestamp_cols <- sapply(data, function(col) inherits(col, 'POSIXct') || inherits(col, 'POSIXlt') || inherits(col, 'Date'))
  
  # Generate a random offset between -max_offset_seconds and max_offset_seconds
  offset <- sample(-max_offset_seconds:max_offset_seconds, size = 1)
  
  # Apply the random offset to all timestamp columns
  data[timestamp_cols] <- lapply(data[timestamp_cols], function(time_col) {
    lubridate::as_datetime(time_col, tz = "UTC") + offset
  })
  
  return(data)
}
