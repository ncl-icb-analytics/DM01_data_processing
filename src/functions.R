# Packages used ----
library(tidyverse)
library(ggrepel)
library(lubridate)
#library(zoo)
library(NCLRtemplates)
library(officer)


# Themes??? ----
# using NCL for now - but should label providers with specific colours for consistency

# Wrangling functions ----

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


## snake case for character variables
text_normalisation <-function(column_name){
  
  name <- str_to_lower(column_name)%>%
    str_squish()%>%
    str_replace_all(" ", "_")
  
  return(name)
  
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

