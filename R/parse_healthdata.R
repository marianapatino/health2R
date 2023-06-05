#' Parse Apple Health Data in Zip format 
#'
#' @param filename filename of the raw data. For Apple Health Data, this should be a zip file
#' @param type type of health data. Currently supports 'Apple'
#' @return data frame with health data
#' @references reference data structure
#' @note note
#' @examples
#' options(tidyverse.quiet = TRUE)
#' library(tidyverse)
#' library(health2R)
#' further examples how to use here 
#' @export
parse_healthdata <- function(filename, type = "apple"){

  
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
  records <- all_records#[1:20000]
  
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


