extract_date <- function(file_name) {
  date_str <- str_extract(file_name, "\\d{6}$|\\d{2}-\\d{2}-\\d{4}|\\d{2}\\.\\d{2}\\.\\d{2}|\\d{8}|\\d{4}-\\d{2}-\\d{2}")
  
  if (!is.na(date_str)) {
    if (str_detect(date_str, "^\\d{6}$")) {
      # Date format without separators, e.g., 230115
      return(as.Date(date_str, format = "%y%m%d"))
    } else if (str_detect(date_str, "^\\d{2}-\\d{2}-\\d{4}$")) {
      # Date format with dashes, e.g., 01-01-2023
      return(as.Date(date_str, format = "%d-%m-%Y"))
    } else if (str_detect(date_str, "^\\d{2}\\.\\d{2}\\.\\d{2}$")) {
      # Date format with dots, e.g., 23.01.08
      return(as.Date(date_str, format = "%y.%m.%d"))
    } else if (str_detect(date_str, "^\\d{8}$")) {
      # Date format without separators, e.g., 20230108
      return(as.Date(date_str, format = "%Y%m%d"))
    } else if (str_detect(date_str, "^\\d{4}-\\d{2}-\\d{2}$")) {
      # Date format with dashes, e.g., 2023-01-08
      return(as.Date(date_str, format = "%Y-%m-%d"))
    }
  }
  
  return(NA)
}



# Function to get the sheet name based on provider name
get_sheet_name <- function(provider_name) {
  sheet_parameters$SheetNames[provider_name]
}