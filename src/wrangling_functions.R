library(tidyverse)
library(readxl)
library(lubridate)

# Adding functions to validate column data types and to normalize column names
columns_of_interest = c("report_date",  "site","day_of_first_auth_date", "tat_full_caldays" )

# Function to validate column data types
validate_column_types <- function(data) {
  # Define the rules for each column. No need to specify a rule for text columns.
  column_rules <- list(
    "tat_full_caldays" = "numeric",  # Example rule: tat should be numeric
    "specimencount" = "numeric", # Example rule: specimen count should be numeric
    "dayoffirstauthdate" = "DateTime"
    # Add more rules for other columns as needed
  )
  
  validation_results <- list()
  
  # Loop through each column
  for (col_name in names(data)) {
    # Check if the column has a rule defined
    if (col_name %in% names(column_rules)) {
      # Get the actual data type of the column
      col_data_type <- class(data[[col_name]])
      # Get the expected data type from the rules
      expected_data_type <- column_rules[[col_name]]
      # Compare each element of col_data_type with expected_data_type
      if (!all(col_data_type == expected_data_type)) {
        # If data types don't match, store the validation result
        validation_results[[col_name]] <- sprintf("Column '%s' has incorrect data type. Expected: %s, Actual: %s", col_name, expected_data_type, unique(col_data_type))
      } else if (expected_data_type == "character") {
        # Check if the column is empty (contains only NA values)
        if (all(is.na(data[[col_name]]))) {
          # If column is empty, store the validation result
          validation_results[[col_name]] <- sprintf("Column '%s' should not be empty.", col_name)
        }
      }
    }
  }
  
  return(validation_results)
}

# more generalised version of your normalisation function
text_normalisation <-function(column_name){
  
  name <- str_to_lower(column_name)%>%
    str_squish()%>%
    str_replace_all(" ", "_")
  
  return(name)
  
}

# Normalize column names: convert to lowercase and remove spaces
normalize_column_names <- function(data) {
  names(data) <- gsub(" ", "", tolower(names(data)))
  return(data)
}


