# trying out functions for error handling

# Begin a transaction
dbBegin(con)

# Error handling with type mismatch handling
tryCatch({
  
  # Get the SQL table schema (for example, SQL Server)
  table_schema <- dbGetQuery(con, "
    SELECT COLUMN_NAME, DATA_TYPE 
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = 'diagnostics_combined_data_2';
  ")
  
  # Define a helper function to match data types between R and SQL
  check_data_type <- function(column, sql_type) {
    if (sql_type == "varchar" && !is.character(column)) {
      stop(sprintf("Data type mismatch: Expected varchar, but got %s in column '%s'.", class(column), deparse(substitute(column))))
    }
    if (sql_type == "int" && !is.numeric(column)) {
      stop(sprintf("Data type mismatch: Expected int, but got %s in column '%s'.", class(column), deparse(substitute(column))))
    }
    if (sql_type == "date" && !inherits(column, "Date")) {
      stop(sprintf("Data type mismatch: Expected date, but got %s in column '%s'.", class(column), deparse(substitute(column))))
    }
    # Add more data type checks as needed (e.g., for float, decimal, etc.)
  }
  
  # Compare R data types with SQL schema
  for (col in colnames(wide_data)) {
    sql_type <- table_schema$DATA_TYPE[table_schema$COLUMN_NAME == col]
    if (length(sql_type) == 1) {  # Ensure column exists in SQL table
      check_data_type(wide_data[[col]], sql_type)
    }
  }
  
  # Separate the wide data into flex and freeze datasets
  wide_flex_data <- wide_data %>% filter(datatype == "flex")
  wide_freeze_data <- wide_data %>% filter(datatype == "freeze")
  
  # Check for existing records in freeze data (continue as in your original code)
  if (nrow(wide_freeze_data) > 0) {
    unique_freeze_entries <- wide_freeze_data %>%
      select(provider_name, reportdate) %>%
      distinct()
    
    # SQL IN clause strings for freeze data
    freeze_provider_names <- paste0("'", unique_freeze_entries$provider_name, "'", collapse = ", ")
    freeze_report_dates <- paste0("'", unique_freeze_entries$reportdate, "'", collapse = ", ")
    
    # Query to check for existing freeze data
    freeze_query <- sprintf(
      "SELECT provider_name, reportdate FROM [Data_Lab_NCL].[dbo].[diagnostics_combined_data_2] 
       WHERE datatype = 'freeze' AND provider_name IN (%s) AND reportdate IN (%s)",
      freeze_provider_names, freeze_report_dates
    )
    
    existing_freeze_data <- dbGetQuery(con, freeze_query)
    
    # If existing freeze data is found, raise an error
    if (nrow(existing_freeze_data) > 0) {
      stop(sprintf(
        "Duplicate freeze data found for provider_name(s): %s on reportdate(s): %s.",
        paste(existing_freeze_data$provider_name, collapse = ", "),
        paste(existing_freeze_data$reportdate, collapse = ", ")
      ))
    }
  }
  
  # Perform deletion and insertion operations
  if (nrow(wide_flex_data) > 0) {
    # Similar code for flex data
    delete_statement <- sprintf(
      "DELETE FROM [Data_Lab_NCL].[dbo].[diagnostics_combined_data_2] 
       WHERE datatype = 'flex' 
       AND provider_name IN (%s)
       AND reportdate IN (%s)",
      flex_provider_names, flex_report_dates
    )
    dbExecute(con, delete_statement)
  }
  
  # Insert data into the SQL table
  if (nrow(wide_freeze_data) > 0) {
    dbAppendTable(con, Id(schema = "dbo", table = "diagnostics_combined_data_2"), wide_freeze_data)
  }
  
  if (nrow(wide_flex_data) > 0) {
    dbAppendTable(con, Id(schema = "dbo", table = "diagnostics_combined_data_2"), wide_flex_data)
  }
  
  # Commit the transaction
  dbCommit(con)
  
}, error = function(e) {
  # Rollback the transaction in case of error
  dbRollback(con)
  stop("Transaction failed: ", e$message)
})

