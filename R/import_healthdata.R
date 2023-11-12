#' Import and Parse Apple Health Data from Zip Archive
#'
#' This function extracts and parses health data from an Apple Health Data zip 
#' file. It reads the contained XML file, parses health records, and returns a 
#' data frame with the health data.
#'
#' @param filename The path to the zip file containing Apple Health Export data.
#' @param datasource The source of the health data, with default set to 'apple'.
#'        Currently, only Apple Health data is supported.
#' @return A data frame containing health data with appropriate data types for 
#'         date-time and numeric fields.
#' @references [Add references here if applicable]
#' @note [Any important notes for the user]
#' @examples
#' # Assuming 'healthdata.zip' is the Apple Health Export zip file in the working directory:
#' \dontrun{
#' options(tidyverse.quiet = TRUE)
#' library(tidyverse)
#' library(xml2)
#' library(lubridate)
#' health_data <- import_healthdata(filename = "healthdata.zip")
#' }
#' @export
import_healthdata <- function(filename, 
                              datasource = "apple"){

  DATETIME_KEYS <- c("startDate", "endDate", "creationDate")
  NUMERIC_KEYS <- c("value")
  OTHER_KEYS <- c("type", "sourceName", "sourceVersion", "unit", "device")
  ALL_KEYS <- c(OTHER_KEYS, DATETIME_KEYS, NUMERIC_KEYS)
  
  zip_file <- filename
  
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
  xml_file <- list.files(tmpdir, pattern = "export\\.xml", full.names = TRUE,recursive = T)
  
  doc <- xml2::read_xml(xml_file)
  all_records <- xml2::xml_find_all(doc, "//Record")

  # here potentially filter rows if too slow
  records <- all_records#[870000:880000]
  
  #####################
  pb <- dplyr::progress_estimated(length(records))
  
    records_data <- 
      purrr::map_dfr(records, 
                     ~{
                       pb$tick()$print()
                       record <- .
                       record_data <- purrr::map(ALL_KEYS, ~ xml2::xml_attr(record, .)) %>%
                         purrr::map_if(is.null, ~NA) %>%
                         as.list() %>%
                         setNames(ALL_KEYS)
                       return(record_data)
                     })
    
    # convert columns to correct types
    records_data <- 
      records_data %>%
      dplyr::mutate(across(DATETIME_KEYS, lubridate::as_datetime))

  return(records_data)
}

# saveRDS(records_data,"dev/data/parsed_healthdata_mp.rds")

