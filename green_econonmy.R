library(foreign)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
library(ggrepel)

# Data from OECD Stats
raw <- read.csv("green_growth.csv") %>% select(-YEA) %>% clean_names() 
three <- c("Germany", "United States", "World")
three_color <- c("#00AFBB", "#E7B800", "#FC4E07")
five <- c("Germany", "United States", "World", "Korea", "Netherlands")
five_color <- c("#00AFBB", "#E7B800", "#FC4E07", "#0000FF", "#800080")
end_year <- 2018
parameter <- "FFS_TTAX"
number <- 3

# Define functions for comparing Germany, US, World
graph <- function(parameter, number) {

  if (number == 3) {  
    subject <- three
    colorf <- three_color
  }
  else {
    subject <- five
    colorf <- five_color
  }

  a <- raw %>% filter(var==parameter, country %in% subject) 
  
  # For printing values at end year
  end <- a %>% filter(year==end_year) %>% 
    mutate(value=round(value,2))
  
  # Basic plot
  plot_three <- ggplot(a, aes(x=year, y=value, color=country, group=country)) + geom_line() +
    theme_classic() +
    scale_colour_manual(values=colorf) +
    labs(color="") +
    theme(legend.position="top",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(hjust=0.5)) +
    geom_text(aes(label=value), data=end, size=3, show.legend=F, 
              check_overlap=T, hjust=0.3, vjust=0.1)

  # Define plot title
  plot_title <- as.character(unique(raw$variable[raw$var==parameter]))
  
  # Add title to the plot 
  plot_three + labs(title=plot_title) 
  
  # Save as image
  file_name <- paste(parameter, number, sep="_")
  ggsave(paste0(file_name, ".png"), width=5, height=3)
  
}

# GDP
graph("GDP_R00", 3)

# CO2 production
graph("CO2_PBEM",3)

# Fossil Fuel support
graph("FFS_TTAX", 3)

# RE support 
graph("RERD_ERD", 3)

# Energy Intensity
graph("NRG_INT", 3)

# Renewable share in electricity
graph("RE_NRG", 3)

graph("ENVRD_GBAORD", 3)



#ODA data
raw <- read.csv("green_oda.csv")%>% select(-YEA) %>% clean_names() 
unique(raw$var)
unique(raw$variable)

graph("ODA_ENV", 3)

# Greenhouse gas emissions data
raw <- read.csv("co2emissions.csv") %>% select(-YEA) %>% clean_names() %>% 
  filter(pollutant=="Greenhouse gases")


graph("TOTAL", 5)
