# Packages used ----
library(tidyverse)
library(ggrepel)
library(lubridate)
library(readxl)
library(NCLRtemplates)
library(officer)


# Themes??? ----
# using NCL for now - but should label providers with specific colours for consistency

# Wrangling functions ----

## snake case for character variables
text_normalisation <-function(column_name){
  
  name <- str_to_lower(column_name)%>%
    str_squish()%>%
    str_replace_all(" ", "_")
  
  return(name)
}

## ingestion of raw data
# Function to process each file
ingest_raw_data_file <- function(file_path) {
  
  # Get the sheet parameters for the specific provider
  provider_name <- text_normalisation(str_extract(basename(file_path), "^[^_]+"))
  
  provider_params <- sheet_parameters %>% 
    filter(Provider == provider_name)
  
  # warning if no parameters are found
  if (nrow(provider_params) == 0) {
    warning("Sheet parameters not found for provider: ", provider_name)
    return(NULL)
  }
  
  # Extract the parameters for reading the Excel sheets
  skip_rows <- provider_params$SkipRows
  range_start <- provider_params$RangeStart
  range_end <- provider_params$RangeEnd
  sheet_name <- provider_params$SheetName
  
  # Get the column names for each provider
  column_names <-  filter(Indicators, Provider == provider_name)$Indicator
  
  # Read the specified sheet, skipping non-tabular rows
  weekly_data <- tryCatch(
    read_excel(file_path, sheet = sheet_name, col_names = FALSE, skip = skip_rows, range = paste0(range_start, ":", range_end)),
    error = function(e) {
      warning("Failed to read sheet for file: ", file_name)
      return(NULL)
    }
  )
  
  if (is.null(weekly_data) || nrow(weekly_data) == 0) {
    warning("No data found in ", file_path)
    return(NULL)
  }
  
  # Assign column names based on provider
  colnames(weekly_data) <- column_names
  
  # Convert columns to numeric, keeping NAs (except the Speciality column)
  weekly_data <- weekly_data %>%
    mutate(across(.cols = -Speciality, .fns = ~ as.numeric(.))) %>%
    pivot_longer(-Speciality, names_to = 'Indicator', values_to = 'Activity') %>%
    drop_na(Speciality)
}

## filter data for plotting
filter_plot_data <- function(data, provider = 'all', speciality_group = 'all', specialty = 'all', indicator = 'all', report_date, duration){
  
  if(speciality_group != 'all'){

    data <- data %>%
    filter(Speciality_Group == {{speciality_group}})
  }else{}
  
  if(specialty != 'all'){
    
    data <- data %>%
      filter(Speciality == {{specialty}})
  }else{}
  
  if(indicator != 'all'){
    
    data <- data %>%
      filter(Indicator == {{indicator}})
  }else{}
  
  if(provider != 'all'){
    
    data <- data %>%
      filter(Provider == {{provider}})
  }else{}
  
  data <- data %>%
    filter(between(Date, ymd({{report_date}})-days({{duration}}), ymd({{report_date}})))%>%
    mutate(Date = as.Date(Date)) %>%
    drop_na(Activity)
  
  return(data)
  
}




## graphing code --- 

line_plot_time <- function(data, plot_var, y_axis_label, title){

ggplot(data, aes(x = Date, y = Activity, group = {{plot_var}}, colour = {{plot_var}})) +
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  labs(
    title = str_wrap(paste0(title),80),
    x = "Date",
    y = paste0(y_axis_label)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  expand_limits(x = max(data$Date+2)) + # nudging to make space for labels
  geom_text_repel(data = subset(data, Date == max(Date)), # reformatting end of line labels
                  aes(color = {{plot_var}}, label = str_wrap({{plot_var}}, 8)),
                  family = "Lato",
                  fontface = "bold",
                  size = 15,
                  direction = "y",
                  nudge_x = 3,
                  nudge_y = 0.1,
                  hjust = 0,
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding =0.3,
                  point.padding = 0.5,
                  segment.curvature = -0.1,
                  segment.ncp = 4,
                  segment.angle = 20, 
                  show.legend=F)+
    theme_nclicb()+
    theme(legend.position = 'none',
          text = element_text(size = 40)
         )+
    scale_colour_ncl()
}

# testing area ----  
#test = filter_plot_data(data = data, indicator = 'patient_cancelled', specialty = 'Magnetic Resonance Imaging', report_date = 20240201, duration = 30)
#line_plot_time(test, Provider, 'Patients cancelled in 3 days', 'Some Graph')

